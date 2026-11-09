#' Index-organized tables with their size
#'
#' IOT data lives in the `IOT - TOP` index. Reads `DBA_SEGMENTS` with
#' `DBA_INDEXES` and `DBA_TABLES`, falling back to `ALL_INDEXES` and
#' `ALL_TABLES`.
#'
#' @inheritParams ora_schema_breakdown
#' @return a data.frame, largest first, with columns `table_name`,
#'   `index_name`, `partitioned`, `segments`, `size_<unit>`,
#'   `max_segment_<unit>` (largest partition) and `overflow_<unit>`.
#'   Attribute `partial` is `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_iot_tables(con, Sys.getenv("ORA_USER"))
#' DBI::dbDisconnect(con)
ora_iot_tables <- function(con, owner, unit = "GB") {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  checkmate::assert_choice(unit, names(ora_units))
  template <- "
    WITH iot AS (
      SELECT i.table_name, i.index_name, i.partitioned,
             COUNT(*) AS segments, SUM(s.bytes) AS size_bytes,
             MAX(s.bytes) AS max_segment_bytes
        FROM dba_segments s
        JOIN {dict}_indexes i
          ON i.owner = s.owner AND i.index_name = s.segment_name
       WHERE s.owner = ?
         AND i.index_type = 'IOT - TOP'
         AND s.segment_type IN ('INDEX', 'INDEX PARTITION',
                                'INDEX SUBPARTITION')
       GROUP BY i.table_name, i.index_name, i.partitioned
    ), ovf AS (
      SELECT t.iot_name AS table_name, SUM(s.bytes) AS overflow_bytes
        FROM dba_segments s
        JOIN {dict}_tables t
          ON t.owner = s.owner AND t.table_name = s.segment_name
       WHERE s.owner = ?
         AND t.iot_type = 'IOT_OVERFLOW'
       GROUP BY t.iot_name
    )
    SELECT iot.table_name, iot.index_name, iot.partitioned, iot.segments,
           iot.size_bytes, iot.max_segment_bytes,
           NVL(ovf.overflow_bytes, 0) AS overflow_bytes
      FROM iot
      LEFT JOIN ovf ON ovf.table_name = iot.table_name
     ORDER BY iot.size_bytes DESC"
  res <- ora_query_dict(
    con,
    template,
    c("INDEXES", "TABLES"),
    params = list(owner, owner)
  )
  res$partitioned <- ora_yes_no(res$partitioned)
  ora_apply_unit(
    res,
    c("size_bytes", "max_segment_bytes", "overflow_bytes"),
    unit
  )
}

#' SQL condition for the partition segments of a table, index or IOT
#'
#' Binds owner, segment name, owner, table name.
#'
#' @return an SQL fragment with `{dict}` placeholders.
#' @noRd
ora_partition_where <- function() {
  "s.owner = ?
   AND (s.segment_name = ?
        OR s.segment_name IN (SELECT i.index_name
                                FROM {dict}_indexes i
                               WHERE i.owner = ?
                                 AND i.table_name = ?
                                 AND i.index_type = 'IOT - TOP'))
   AND s.partition_name IS NOT NULL"
}

#' Size per partition
#'
#' For an IOT, pass the table name: its `IOT - TOP` index partitions are
#' used. Reads `DBA_SEGMENTS` with `DBA_INDEXES`, falling back to
#' `ALL_INDEXES`.
#'
#' @inheritParams ora_schema_breakdown
#' @param segment_name the table, index or IOT name.
#' @param n the number of largest partitions to return, or `NULL` for all.
#' @return a data.frame, largest first, with columns `segment_name`,
#'   `partition_name`, `segment_type`, `tablespace_name` and `size_<unit>`.
#'   Attribute `partial` is `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_partition_sizes(con, Sys.getenv("ORA_USER"), "MY_TABLE", n = 10)
#' DBI::dbDisconnect(con)
ora_partition_sizes <- function(
  con,
  owner,
  segment_name,
  n = NULL,
  unit = "GB"
) {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  segment_name <- ora_as_name(segment_name, len = 1L)
  checkmate::assert_count(n, positive = TRUE, null.ok = TRUE)
  checkmate::assert_choice(unit, names(ora_units))
  params <- list(owner, segment_name, owner, segment_name)
  limit <- ""
  if (!is.null(n)) {
    limit <- "FETCH FIRST ? ROWS ONLY"
    params <- c(params, list(as.integer(n)))
  }
  template <- paste(
    "SELECT s.segment_name, s.partition_name, s.segment_type,
            s.tablespace_name, s.bytes AS size_bytes
       FROM dba_segments s
      WHERE",
    ora_partition_where(),
    "ORDER BY s.bytes DESC, s.partition_name",
    limit
  )
  res <- ora_query_dict(con, template, "INDEXES", params = params)
  ora_apply_unit(res, "size_bytes", unit)
}

#' Summary of the partitions of a segment
#'
#' For an IOT, pass the table name. Reads `DBA_SEGMENTS` with
#' `DBA_INDEXES`, falling back to `ALL_INDEXES`.
#'
#' @inheritParams ora_partition_sizes
#' @return a one-row data.frame with columns `partitions`, `min_partition`,
#'   `max_partition` (by name), `total_<unit>`, `avg_<unit>` and `max_<unit>`.
#'   Attribute `partial` is `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_partition_summary(con, Sys.getenv("ORA_USER"), "MY_TABLE")
#' DBI::dbDisconnect(con)
ora_partition_summary <- function(con, owner, segment_name, unit = "GB") {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  segment_name <- ora_as_name(segment_name, len = 1L)
  checkmate::assert_choice(unit, names(ora_units))
  template <- paste(
    "SELECT COUNT(*) AS partitions,
            MIN(s.partition_name) AS min_partition,
            MAX(s.partition_name) AS max_partition,
            SUM(s.bytes) AS total_bytes,
            AVG(s.bytes) AS avg_bytes,
            MAX(s.bytes) AS max_bytes
       FROM dba_segments s
      WHERE",
    ora_partition_where()
  )
  res <- ora_query_dict(
    con,
    template,
    "INDEXES",
    params = list(owner, segment_name, owner, segment_name)
  )
  ora_apply_unit(res, c("total_bytes", "avg_bytes", "max_bytes"), unit)
}

#' Estimate the data size of a table from statistics
#'
#' Estimate is `num_rows * avg_row_len`. Locked statistics are never
#' refreshed and staleness is only known after Oracle flushes its DML
#' counts, so check `reliable`. Reads `DBA_TAB_STATISTICS`, falling back to
#' `ALL_TAB_STATISTICS`, and `DBA_SEGMENTS`.
#'
#' @inheritParams ora_schema_breakdown
#' @param table the table name.
#' @return a one-row data.frame with columns `table_name`, `num_rows`,
#'   `avg_row_len`, `estimated_<unit>`, `segment_<unit>` (actual,
#'   IOT-aware), `last_analyzed`, `stats_locked`, `stale_stats` and
#'   `reliable` (statistics present, not stale and not locked). Attribute
#'   `partial` is `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' try(ora_estimate_data_size(con, Sys.getenv("ORA_USER"), "MY_TABLE"))
#' DBI::dbDisconnect(con)
ora_estimate_data_size <- function(con, owner, table, unit = "GB") {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  table <- ora_as_name(table, len = 1L)
  checkmate::assert_choice(unit, names(ora_units))
  template <- "
    SELECT st.table_name, st.num_rows, st.avg_row_len,
           st.num_rows * st.avg_row_len AS estimated_bytes,
           (SELECT NVL(SUM(s.bytes), 0)
              FROM dba_segments s
             WHERE s.owner = st.owner
               AND (s.segment_name = st.table_name
                    OR s.segment_name IN (
                         SELECT i.index_name
                           FROM {dict}_indexes i
                          WHERE i.owner = st.owner
                            AND i.table_name = st.table_name
                            AND i.index_type = 'IOT - TOP'))
           ) AS segment_bytes,
           st.last_analyzed, st.stattype_locked, st.stale_stats
      FROM {dict}_tab_statistics st
     WHERE st.owner = ?
       AND st.table_name = ?
       AND st.object_type = 'TABLE'"
  res <- ora_query_dict(
    con,
    template,
    c("TAB_STATISTICS", "INDEXES"),
    params = list(owner, table)
  )
  if (nrow(res) == 0L) {
    stop(
      sprintf("Table %s.%s not found or not visible.", owner, table),
      call. = FALSE
    )
  }
  res$stats_locked <- !is.na(res$stattype_locked)
  res$stattype_locked <- NULL
  res$stale_stats <- ora_yes_no(res$stale_stats)
  res$reliable <- !is.na(res$num_rows) &
    res$stale_stats %in% FALSE &
    !res$stats_locked
  ora_apply_unit(res, c("estimated_bytes", "segment_bytes"), unit)
}

#' Count the rows of a table or partition
#'
#' Runs `SELECT COUNT(*)`, which reads the whole table or partition and can
#' be slow on large tables. Needs `SELECT` on the table.
#'
#' @inheritParams ora_estimate_data_size
#' @param partition the partition name, or `NULL` for the whole table.
#' @return a one-row data.frame with columns `owner`, `table_name`,
#'   `partition_name`, `num_rows` and `seconds` (elapsed time).
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_count_rows(con, "SYS", "DUAL")
#' DBI::dbDisconnect(con)
ora_count_rows <- function(con, owner, table, partition = NULL) {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  table <- ora_as_name(table, len = 1L)
  target <- ora_quote_name(paste(owner, table, sep = "."))
  partition_name <- NA_character_
  if (!is.null(partition)) {
    partition_name <- ora_as_name(partition, len = 1L)
    target <- sprintf(
      "%s PARTITION (%s)",
      target,
      ora_quote_ident(partition_name)
    )
  }
  message(sprintf("Counting rows of %s, this can be slow.", target))
  start <- Sys.time()
  res <- ora_query(
    con,
    sprintf("SELECT COUNT(*) AS num_rows FROM %s", target),
    object = paste(owner, table, sep = ".")
  )
  seconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  data.frame(
    owner = owner,
    table_name = table,
    partition_name = partition_name,
    num_rows = as.numeric(res$num_rows),
    seconds = round(seconds, 2L)
  )
}
