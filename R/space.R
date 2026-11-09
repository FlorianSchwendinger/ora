#' How full the database is
#'
#' Used space from `DBA_TABLESPACE_USAGE_METRICS`, allocated and maximum
#' (autoextend-aware) from `DBA_DATA_FILES`. Temporary tablespaces are
#' excluded.
#'
#' @inheritParams ora_version
#' @param unit a size unit, one of `"B"`, `"KB"`, `"MB"`, `"GB"` or `"TB"`.
#' @return a one-row data.frame with columns `used_<unit>`,
#'   `allocated_<unit>`, `max_<unit>`, `free_<unit>` (max minus used) and
#'   `used_pct` (of max).
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_db_fullness(con)
#' DBI::dbDisconnect(con)
ora_db_fullness <- function(con, unit = "GB") {
  assert_dbi_con(con)
  checkmate::assert_choice(unit, names(ora_units))
  sql <- "
    WITH used AS (
      SELECT SUM(m.used_space * t.block_size) AS used_bytes
        FROM dba_tablespace_usage_metrics m
        JOIN dba_tablespaces t ON t.tablespace_name = m.tablespace_name
       WHERE t.contents <> 'TEMPORARY'
    ), files AS (
      SELECT SUM(bytes) AS allocated_bytes,
             SUM(CASE WHEN autoextensible = 'YES'
                      THEN GREATEST(maxbytes, bytes)
                      ELSE bytes END) AS max_bytes
        FROM dba_data_files
    )
    SELECT u.used_bytes, f.allocated_bytes, f.max_bytes,
           f.max_bytes - u.used_bytes AS free_bytes,
           ROUND(100 * u.used_bytes / NULLIF(f.max_bytes, 0), 2) AS used_pct
      FROM used u CROSS JOIN files f"
  res <- ora_query(con, sql, object = "DBA_TABLESPACE_USAGE_METRICS")
  ora_apply_unit(
    res,
    c("used_bytes", "allocated_bytes", "max_bytes", "free_bytes"),
    unit
  )
}

#' Space usage per tablespace
#'
#' Permanent and undo tablespaces from `DBA_TABLESPACE_USAGE_METRICS`,
#' `DBA_TABLESPACES` and `DBA_DATA_FILES`. See [ora_temp_usage()] for
#' temporary tablespaces.
#'
#' @inheritParams ora_db_fullness
#' @return a data.frame, fullest first, with columns `tablespace_name`,
#'   `contents`, `status`, `bigfile`, `used_<unit>`, `allocated_<unit>`,
#'   `max_<unit>`, `free_<unit>` and `used_pct` (of max).
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_tablespace_usage(con)
#' DBI::dbDisconnect(con)
ora_tablespace_usage <- function(con, unit = "GB") {
  assert_dbi_con(con)
  checkmate::assert_choice(unit, names(ora_units))
  sql <- "
    SELECT m.tablespace_name, t.contents, t.status, t.bigfile,
           m.used_space * t.block_size AS used_bytes,
           f.allocated_bytes,
           m.tablespace_size * t.block_size AS max_bytes,
           (m.tablespace_size - m.used_space) * t.block_size AS free_bytes,
           ROUND(m.used_percent, 2) AS used_pct
      FROM dba_tablespace_usage_metrics m
      JOIN dba_tablespaces t ON t.tablespace_name = m.tablespace_name
      LEFT JOIN (SELECT tablespace_name, SUM(bytes) AS allocated_bytes
                   FROM dba_data_files
                  GROUP BY tablespace_name) f
        ON f.tablespace_name = m.tablespace_name
     WHERE t.contents <> 'TEMPORARY'
     ORDER BY m.used_percent DESC"
  res <- ora_query(con, sql, object = "DBA_TABLESPACE_USAGE_METRICS")
  ora_apply_unit(
    res,
    c("used_bytes", "allocated_bytes", "max_bytes", "free_bytes"),
    unit
  )
}

#' Space usage of temporary tablespaces
#'
#' Reads `DBA_TABLESPACE_USAGE_METRICS` and `DBA_TABLESPACES`.
#'
#' @inheritParams ora_db_fullness
#' @return a data.frame, fullest first, with columns `tablespace_name`,
#'   `status`, `used_<unit>`, `max_<unit>`, `free_<unit>` and `used_pct`
#'   (of max).
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_temp_usage(con)
#' DBI::dbDisconnect(con)
ora_temp_usage <- function(con, unit = "GB") {
  assert_dbi_con(con)
  checkmate::assert_choice(unit, names(ora_units))
  sql <- "
    SELECT m.tablespace_name, t.status,
           m.used_space * t.block_size AS used_bytes,
           m.tablespace_size * t.block_size AS max_bytes,
           (m.tablespace_size - m.used_space) * t.block_size AS free_bytes,
           ROUND(m.used_percent, 2) AS used_pct
      FROM dba_tablespace_usage_metrics m
      JOIN dba_tablespaces t ON t.tablespace_name = m.tablespace_name
     WHERE t.contents = 'TEMPORARY'
     ORDER BY m.used_percent DESC"
  res <- ora_query(con, sql, object = "DBA_TABLESPACE_USAGE_METRICS")
  ora_apply_unit(res, c("used_bytes", "max_bytes", "free_bytes"), unit)
}

#' Fast recovery area usage
#'
#' Reads `V$RECOVERY_FILE_DEST`. Check it before large moves, which write
#' redo and archive logs. Without access this raises `ora_no_privilege`.
#'
#' @inheritParams ora_db_fullness
#' @return a one-row data.frame with columns `name`, `limit_<unit>`,
#'   `used_<unit>`, `reclaimable_<unit>`, `free_<unit>` (limit minus used
#'   plus reclaimable), `used_pct` and `number_of_files`. No row if no FRA
#'   is set.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' try(ora_fra_usage(con))
#' DBI::dbDisconnect(con)
ora_fra_usage <- function(con, unit = "GB") {
  assert_dbi_con(con)
  checkmate::assert_choice(unit, names(ora_units))
  sql <- "
    SELECT name,
           space_limit AS limit_bytes,
           space_used AS used_bytes,
           space_reclaimable AS reclaimable_bytes,
           space_limit - space_used + space_reclaimable AS free_bytes,
           ROUND(100 * space_used / NULLIF(space_limit, 0), 2) AS used_pct,
           number_of_files
      FROM v$recovery_file_dest
     WHERE name IS NOT NULL"
  res <- ora_query(con, sql, object = "V$RECOVERY_FILE_DEST")
  ora_apply_unit(
    res,
    c("limit_bytes", "used_bytes", "reclaimable_bytes", "free_bytes"),
    unit
  )
}
