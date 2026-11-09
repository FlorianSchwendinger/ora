#' Instance startup time and uptime
#'
#' Statistics in `V$SEGMENT_STATISTICS` count from startup, so check the
#' uptime before trusting [ora_untouched_tables()]. Reads `V$INSTANCE`.
#'
#' @inheritParams ora_version
#' @return a one-row data.frame with columns `instance_name`, `host_name`,
#'   `status`, `startup_time` and `uptime_days`.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' try(ora_instance_uptime(con))
#' DBI::dbDisconnect(con)
ora_instance_uptime <- function(con) {
  sql <- "
    SELECT instance_name, host_name, status, startup_time,
           ROUND(SYSDATE - startup_time, 2) AS uptime_days
      FROM v$instance"
  ora_query(con, sql, object = "V$INSTANCE")
}

#' Tables without logical reads since instance startup
#'
#' Counts reads on table segments and on `IOT - TOP` index segments. Only
#' meaningful after a long uptime, see [ora_instance_uptime()]. Reads
#' `V$SEGMENT_STATISTICS` and `DBA_SEGMENTS` with `DBA_INDEXES`, falling
#' back to `ALL_INDEXES`.
#'
#' @inheritParams ora_schema_breakdown
#' @return a data.frame, largest first, with columns `table_name`,
#'   `segments`, `size_<unit>`, `logical_reads` (always 0) and
#'   `block_changes`. Attribute `partial` is `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' try(ora_untouched_tables(con, Sys.getenv("ORA_USER")))
#' DBI::dbDisconnect(con)
ora_untouched_tables <- function(con, owner, unit = "GB") {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  checkmate::assert_choice(unit, names(ora_units))
  ora_require_views(con, "V$SEGMENT_STATISTICS")
  template <- "
    WITH tseg AS (
      SELECT s.segment_name AS table_name, s.segment_name AS object_name,
             s.partition_name, s.bytes
        FROM dba_segments s
       WHERE s.owner = ?
         AND s.segment_type IN ('TABLE', 'TABLE PARTITION',
                                'TABLE SUBPARTITION')
      UNION ALL
      SELECT i.table_name, s.segment_name, s.partition_name, s.bytes
        FROM dba_segments s
        JOIN {dict}_indexes i
          ON i.owner = s.owner AND i.index_name = s.segment_name
       WHERE s.owner = ?
         AND i.index_type = 'IOT - TOP'
         AND s.segment_type IN ('INDEX', 'INDEX PARTITION',
                                'INDEX SUBPARTITION')
    ), stats AS (
      SELECT object_name, subobject_name,
             SUM(CASE WHEN statistic_name = 'logical reads'
                      THEN value ELSE 0 END) AS logical_reads,
             SUM(CASE WHEN statistic_name = 'db block changes'
                      THEN value ELSE 0 END) AS block_changes
        FROM v$segment_statistics
       WHERE owner = ?
         AND statistic_name IN ('logical reads', 'db block changes')
       GROUP BY object_name, subobject_name
    )
    SELECT t.table_name, COUNT(*) AS segments, SUM(t.bytes) AS size_bytes,
           NVL(SUM(st.logical_reads), 0) AS logical_reads,
           NVL(SUM(st.block_changes), 0) AS block_changes
      FROM tseg t
      LEFT JOIN stats st
        ON st.object_name = t.object_name
       AND NVL(st.subobject_name, '-') = NVL(t.partition_name, '-')
     GROUP BY t.table_name
    HAVING NVL(SUM(st.logical_reads), 0) = 0
     ORDER BY size_bytes DESC"
  res <- ora_query_dict(
    con,
    template,
    "INDEXES",
    params = list(owner, owner, owner)
  )
  ora_apply_unit(res, "size_bytes", unit)
}

#' Indexes never used according to index usage tracking
#'
#' Leaves out primary key, unique constraint, LOB and `IOT - TOP` indexes.
#' Tracking is sampled and flushed every 15 minutes, and indexes on
#' foreign keys may still be needed against locking. Reads
#' `DBA_INDEX_USAGE` and `DBA_SEGMENTS` with `DBA_INDEXES`,
#' `DBA_CONSTRAINTS` and `DBA_OBJECTS`, falling back to the `ALL_*` views.
#'
#' @inheritParams ora_schema_breakdown
#' @return a data.frame, largest first, with columns `index_name`,
#'   `table_name`, `index_type`, `uniqueness`, `created`, `segments` and
#'   `size_<unit>`. Attribute `partial` is `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' try(ora_unused_indexes(con, Sys.getenv("ORA_USER")))
#' DBI::dbDisconnect(con)
ora_unused_indexes <- function(con, owner, unit = "GB") {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  checkmate::assert_choice(unit, names(ora_units))
  ora_require_views(con, "DBA_INDEX_USAGE")
  template <- "
    SELECT i.index_name, i.table_name, i.index_type, i.uniqueness,
           o.created, COUNT(*) AS segments, SUM(s.bytes) AS size_bytes
      FROM {dict}_indexes i
      JOIN dba_segments s
        ON s.owner = i.owner AND s.segment_name = i.index_name
      LEFT JOIN {dict}_objects o
        ON o.owner = i.owner AND o.object_name = i.index_name
       AND o.object_type = 'INDEX'
     WHERE i.owner = ?
       AND i.index_type NOT IN ('LOB', 'IOT - TOP')
       AND NOT EXISTS (SELECT 1
                         FROM dba_index_usage u
                        WHERE u.owner = i.owner
                          AND u.name = i.index_name)
       AND NOT EXISTS (SELECT 1
                         FROM {dict}_constraints c
                        WHERE c.owner = i.owner
                          AND c.index_name = i.index_name
                          AND c.constraint_type IN ('P', 'U'))
     GROUP BY i.index_name, i.table_name, i.index_type, i.uniqueness,
              o.created
     ORDER BY size_bytes DESC"
  res <- ora_query_dict(
    con,
    template,
    c("INDEXES", "OBJECTS", "CONSTRAINTS"),
    params = list(owner)
  )
  ora_apply_unit(res, "size_bytes", unit)
}

#' DML counts since the last statistics
#'
#' Oracle flushes the counts from memory periodically, so recent changes may
#' be missing. Reads `DBA_TAB_MODIFICATIONS` and `DBA_TAB_STATISTICS`,
#' falling back to the `ALL_*` views.
#'
#' @inheritParams ora_estimate_data_size
#' @param table the table name, or `NULL` for all tables of `owner`.
#' @return a data.frame, latest first, with columns `table_name`,
#'   `partition_name`, `subpartition_name`, `inserts`, `updates`,
#'   `deletes`, `truncated`, `num_rows` (from statistics), `pct_changed`
#'   and `timestamp`. Attribute `partial` is `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_table_modifications(con, Sys.getenv("ORA_USER"))
#' DBI::dbDisconnect(con)
ora_table_modifications <- function(con, owner, table = NULL) {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  params <- list(owner)
  table_filter <- ""
  if (!is.null(table)) {
    params <- c(params, list(ora_as_name(table, len = 1L)))
    table_filter <- "AND m.table_name = ?"
  }
  template <- paste(
    "SELECT m.table_name, m.partition_name, m.subpartition_name,
            m.inserts, m.updates, m.deletes, m.truncated, st.num_rows,
            ROUND(100 * (m.inserts + m.updates + m.deletes)
                  / NULLIF(st.num_rows, 0), 2) AS pct_changed,
            m.timestamp
       FROM {dict}_tab_modifications m
       LEFT JOIN {dict}_tab_statistics st
         ON st.owner = m.table_owner
        AND st.table_name = m.table_name
        AND NVL(st.partition_name, '-') = NVL(m.partition_name, '-')
        AND NVL(st.subpartition_name, '-') = NVL(m.subpartition_name, '-')
      WHERE m.table_owner = ?",
    table_filter,
    "ORDER BY m.timestamp DESC, m.table_name, m.partition_name"
  )
  res <- ora_query_dict(
    con,
    template,
    c("TAB_MODIFICATIONS", "TAB_STATISTICS"),
    params = params
  )
  res$truncated <- ora_yes_no(res$truncated)
  res
}
