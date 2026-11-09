#' Parse partition `HIGH_VALUE` text to dates
#'
#' Handles `TO_DATE(...)` and `TIMESTAMP '...'` bounds. `MAXVALUE`,
#' numbers and other keys give `NA`.
#'
#' @param x a character vector of `HIGH_VALUE` texts.
#' @return a Date vector.
#' @noRd
ora_parse_high_value <- function(x) {
  checkmate::assert_character(x)
  m <- regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  res <- rep(NA_character_, length(x))
  hit <- !is.na(m) & m > 0L
  res[hit] <- regmatches(x, m)
  as.Date(res)
}

#' Partition bounds of a table
#'
#' Reads `HIGH_VALUE` (a `LONG` column) and turns date bounds into dates,
#' so old partitions can be picked by date. Size comes from table segments
#' or, for an IOT, its `IOT - TOP` index partitions. Reads
#' `DBA_TAB_PARTITIONS` with `DBA_INDEXES`, falling back to the `ALL_*`
#' views, and `DBA_SEGMENTS`.
#'
#' @inheritParams ora_estimate_data_size
#' @return a data.frame, by position, with columns `partition_name`,
#'   `partition_position`, `low_date` (bound of the previous partition),
#'   `high_date` (exclusive, `NA` for `MAXVALUE` or non-date keys),
#'   `interval` (created by interval partitioning), `size_<unit>` and
#'   `high_value` (the raw text). Attribute `partial` is `TRUE` after a
#'   fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' bounds <- ora_partition_bounds(con, Sys.getenv("ORA_USER"), "MY_TABLE")
#' bounds[bounds$high_date <= as.Date("2021-01-01"), ]
#' DBI::dbDisconnect(con)
ora_partition_bounds <- function(con, owner, table, unit = "GB") {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  table <- ora_as_name(table, len = 1L)
  checkmate::assert_choice(unit, names(ora_units))
  # HIGH_VALUE is LONG: no functions on it, and it must be the last column.
  template <- "
    SELECT p.partition_name, p.partition_position, p.interval,
           (SELECT NVL(SUM(s.bytes), 0)
              FROM dba_segments s
             WHERE s.owner = p.table_owner
               AND s.partition_name = p.partition_name
               AND (s.segment_name = p.table_name
                    OR s.segment_name IN (
                         SELECT i.index_name
                           FROM {dict}_indexes i
                          WHERE i.owner = p.table_owner
                            AND i.table_name = p.table_name
                            AND i.index_type = 'IOT - TOP'))
           ) AS size_bytes,
           p.high_value
      FROM {dict}_tab_partitions p
     WHERE p.table_owner = ? AND p.table_name = ?
     ORDER BY p.partition_position"
  res <- ora_query_dict(
    con,
    template,
    c("TAB_PARTITIONS", "INDEXES"),
    params = list(owner, table)
  )
  partial <- attr(res, "partial")
  high <- ora_parse_high_value(as.character(res$high_value))
  out <- data.frame(
    partition_name = res$partition_name,
    partition_position = res$partition_position,
    low_date = c(as.Date(NA), high)[seq_along(high)],
    high_date = high,
    interval = ora_yes_no(res$interval),
    size_bytes = res$size_bytes,
    high_value = as.character(res$high_value)
  )
  out <- ora_apply_unit(out, "size_bytes", unit)
  attr(out, "partial") <- partial
  out
}

#' Largest segments
#'
#' Across all schemas or within one. Partitions of a segment are summed.
#' Reads `DBA_SEGMENTS`.
#'
#' @inheritParams ora_schema_sizes
#' @param owner the schema name, or `NULL` for all schemas.
#' @param n the number of segments to return.
#' @param include_oracle a flag, keep Oracle-maintained schemas such as
#'   `SYS`. Ignored when `owner` is given.
#' @return a data.frame, largest first, with columns `owner`,
#'   `segment_name`, `segment_type` (without `PARTITION`), `partitions`,
#'   `tablespaces` and `size_<unit>`.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_top_segments(con, n = 10)
#' ora_top_segments(con, Sys.getenv("ORA_USER"), n = 5, unit = "MB")
#' DBI::dbDisconnect(con)
ora_top_segments <- function(
  con,
  owner = NULL,
  n = 20L,
  unit = "GB",
  include_oracle = FALSE
) {
  assert_dbi_con(con)
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_choice(unit, names(ora_units))
  checkmate::assert_flag(include_oracle)
  params <- list()
  if (!is.null(owner)) {
    params <- list(ora_as_name(owner, len = 1L))
    filter <- "owner = ?"
  } else if (include_oracle) {
    filter <- "1 = 1"
  } else {
    filter <- ora_owner_filter("owner")
  }
  sql <- sprintf(
    "SELECT owner, segment_name,
            REGEXP_REPLACE(segment_type, ' (SUB)?PARTITION$', '')
              AS segment_type,
            SUM(CASE WHEN partition_name IS NULL THEN 0 ELSE 1 END)
              AS partitions,
            COUNT(DISTINCT tablespace_name) AS tablespaces,
            SUM(bytes) AS size_bytes
       FROM dba_segments
      WHERE %s
      GROUP BY owner, segment_name,
               REGEXP_REPLACE(segment_type, ' (SUB)?PARTITION$', '')
      ORDER BY size_bytes DESC
      FETCH FIRST ? ROWS ONLY",
    filter
  )
  res <- ora_query(
    con,
    sql,
    params = c(params, list(as.integer(n))),
    object = "DBA_SEGMENTS"
  )
  ora_apply_unit(res, "size_bytes", unit)
}

#' Unusable indexes and index partitions
#'
#' Check after moving or dropping partitions. Rebuild with
#' [ora_sql_rebuild_index()]. Reads `DBA_INDEXES`, `DBA_IND_PARTITIONS`
#' and `DBA_IND_SUBPARTITIONS`, falling back to the `ALL_*` views.
#'
#' @inheritParams ora_schema_breakdown
#' @return a data.frame with columns `index_name`, `table_name`,
#'   `partition_name` (`NA` for a whole index) and `object_level`
#'   (`INDEX`, `PARTITION` or `SUBPARTITION`). Attribute `partial` is `TRUE`
#'   after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_unusable_indexes(con, Sys.getenv("ORA_USER"))
#' DBI::dbDisconnect(con)
ora_unusable_indexes <- function(con, owner) {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  template <- "
    SELECT i.index_name, i.table_name,
           CAST(NULL AS VARCHAR2(128)) AS partition_name,
           'INDEX' AS object_level
      FROM {dict}_indexes i
     WHERE i.owner = ? AND i.status = 'UNUSABLE'
    UNION ALL
    SELECT p.index_name, i.table_name, p.partition_name, 'PARTITION'
      FROM {dict}_ind_partitions p
      JOIN {dict}_indexes i
        ON i.owner = p.index_owner AND i.index_name = p.index_name
     WHERE p.index_owner = ? AND p.status = 'UNUSABLE'
    UNION ALL
    SELECT s.index_name, i.table_name, s.subpartition_name, 'SUBPARTITION'
      FROM {dict}_ind_subpartitions s
      JOIN {dict}_indexes i
        ON i.owner = s.index_owner AND i.index_name = s.index_name
     WHERE s.index_owner = ? AND s.status = 'UNUSABLE'
     ORDER BY 2, 1, 3"
  ora_query_dict(
    con,
    template,
    c("INDEXES", "IND_PARTITIONS", "IND_SUBPARTITIONS"),
    params = list(owner, owner, owner)
  )
}

#' Estimated versus actual size per partition
#'
#' Estimate is `num_rows * avg_row_len` from partition statistics. Shrink
#' cannot go below the first extent, so `reclaimable` is the segment minus
#' the larger of first extent and estimate: an upper bound of what a shrink
#' frees, as block overhead is ignored. Reads `DBA_TAB_STATISTICS` with
#' `DBA_INDEXES`, falling back to the `ALL_*` views, and `DBA_SEGMENTS`.
#'
#' @inheritParams ora_estimate_data_size
#' @return a data.frame, by position, with columns `partition_name`,
#'   `partition_position`, `num_rows`, `avg_row_len`, `estimated_<unit>`,
#'   `segment_<unit>`, `waste_<unit>` (segment minus estimate), `waste_pct`,
#'   `initial_<unit>` (first extent), `reclaimable_<unit>`, `last_analyzed`,
#'   `stats_locked`, `stale_stats` and `reliable`. Attribute `partial` is
#'   `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' est <- ora_partition_estimates(con, Sys.getenv("ORA_USER"), "MY_TABLE")
#' est[order(-est$reclaimable_gb), ]
#' DBI::dbDisconnect(con)
ora_partition_estimates <- function(con, owner, table, unit = "GB") {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  table <- ora_as_name(table, len = 1L)
  checkmate::assert_choice(unit, names(ora_units))
  template <- "
    SELECT st.partition_name, st.partition_position, st.num_rows,
           st.avg_row_len,
           st.num_rows * st.avg_row_len AS estimated_bytes,
           NVL(sg.segment_bytes, 0) AS segment_bytes,
           NVL(sg.initial_bytes, 0) AS initial_bytes,
           st.last_analyzed, st.stattype_locked, st.stale_stats
      FROM {dict}_tab_statistics st
      LEFT JOIN (
        SELECT s.partition_name, SUM(s.bytes) AS segment_bytes,
               MAX(s.initial_extent) AS initial_bytes
          FROM dba_segments s
         WHERE s.owner = ?
           AND (s.segment_name = ?
                OR s.segment_name IN (
                     SELECT i.index_name
                       FROM {dict}_indexes i
                      WHERE i.owner = ?
                        AND i.table_name = ?
                        AND i.index_type = 'IOT - TOP'))
         GROUP BY s.partition_name
      ) sg ON sg.partition_name = st.partition_name
     WHERE st.owner = ?
       AND st.table_name = ?
       AND st.object_type = 'PARTITION'
     ORDER BY st.partition_position"
  res <- ora_query_dict(
    con,
    template,
    c("TAB_STATISTICS", "INDEXES"),
    params = list(owner, table, owner, table, owner, table)
  )
  partial <- attr(res, "partial")
  res$waste_bytes <- res$segment_bytes - res$estimated_bytes
  res$reclaimable_bytes <- ora_reclaimable(
    res$segment_bytes,
    res$initial_bytes,
    res$estimated_bytes
  )
  res$waste_pct <- round(
    100 *
      res$waste_bytes /
      ifelse(res$segment_bytes > 0, res$segment_bytes, NA),
    2L
  )
  res$stats_locked <- !is.na(res$stattype_locked)
  res$stale_stats <- ora_yes_no(res$stale_stats)
  res$reliable <- !is.na(res$num_rows) &
    res$stale_stats %in% FALSE &
    !res$stats_locked
  cols <- c(
    "partition_name",
    "partition_position",
    "num_rows",
    "avg_row_len",
    "estimated_bytes",
    "segment_bytes",
    "waste_bytes",
    "waste_pct",
    "initial_bytes",
    "reclaimable_bytes",
    "last_analyzed",
    "stats_locked",
    "stale_stats",
    "reliable"
  )
  res <- res[, cols]
  res <- ora_apply_unit(
    res,
    c(
      "estimated_bytes",
      "segment_bytes",
      "waste_bytes",
      "initial_bytes",
      "reclaimable_bytes"
    ),
    unit
  )
  attr(res, "partial") <- partial
  res
}

#' Space a shrink can free at most
#'
#' @param segment the segment sizes in bytes.
#' @param initial the first extent sizes in bytes.
#' @param estimated the estimated data sizes in bytes.
#' @return the segment minus the larger of first extent and estimate, at
#'   least 0, `NA` where the estimate is `NA`.
#' @noRd
ora_reclaimable <- function(segment, initial, estimated) {
  pmax(segment - pmax(initial, estimated), 0)
}

#' Lock mode names of `V$LOCKED_OBJECT.LOCKED_MODE`
#' @noRd
ora_lock_modes <- c(
  "0" = "NONE",
  "1" = "NULL",
  "2" = "ROW SHARE",
  "3" = "ROW EXCLUSIVE",
  "4" = "SHARE",
  "5" = "SHARE ROW EXCLUSIVE",
  "6" = "EXCLUSIVE"
)

#' Sessions holding locks on tables
#'
#' DDL on a locked table fails with ORA-00054, so check before
#' [ora_run_sql()]. Reads `V$LOCKED_OBJECT` and `V$SESSION` with
#' `DBA_OBJECTS`, falling back to `ALL_OBJECTS`.
#'
#' @inheritParams ora_schema_breakdown
#' @param table the table name, or `NULL` for all tables of `owner`.
#' @return a data.frame with columns `object_name`, `subobject_name`,
#'   `object_type`, `locked_mode`, `sid`, `serial`, `username`, `osuser`,
#'   `machine`, `program`, `status` and `logon_time`. Attribute `partial` is
#'   `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' try(ora_table_locks(con, Sys.getenv("ORA_USER")))
#' DBI::dbDisconnect(con)
ora_table_locks <- function(con, owner, table = NULL) {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  ora_require_views(con, c("V$LOCKED_OBJECT", "V$SESSION"))
  params <- list(owner)
  table_filter <- ""
  if (!is.null(table)) {
    params <- c(params, list(ora_as_name(table, len = 1L)))
    table_filter <- "AND o.object_name = ?"
  }
  template <- paste(
    "SELECT o.object_name, o.subobject_name, o.object_type,
            lo.locked_mode, s.sid, s.serial# AS serial, s.username,
            s.osuser, s.machine, s.program, s.status, s.logon_time
       FROM v$locked_object lo
       JOIN {dict}_objects o ON o.object_id = lo.object_id
       JOIN v$session s ON s.sid = lo.session_id
      WHERE o.owner = ?",
    table_filter,
    "ORDER BY o.object_name, o.subobject_name, s.sid"
  )
  res <- ora_query_dict(con, template, "OBJECTS", params = params)
  res$locked_mode <- unname(ora_lock_modes[as.character(res$locked_mode)])
  res
}
