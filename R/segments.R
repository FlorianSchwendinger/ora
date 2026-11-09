#' Size per schema
#'
#' Reads `DBA_SEGMENTS`.
#'
#' @inheritParams ora_version
#' @param unit a size unit, one of `"B"`, `"KB"`, `"MB"`, `"GB"` or `"TB"`.
#' @param include_oracle a flag, keep Oracle-maintained schemas such as
#'   `SYS`.
#' @return a data.frame, largest first, with columns `owner`, `segments`,
#'   `size_<unit>` and `pct` (of the listed total).
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_schema_sizes(con)
#' DBI::dbDisconnect(con)
ora_schema_sizes <- function(con, unit = "GB", include_oracle = FALSE) {
  assert_dbi_con(con)
  checkmate::assert_choice(unit, names(ora_units))
  checkmate::assert_flag(include_oracle)
  filter <- if (include_oracle) "1 = 1" else ora_owner_filter("owner")
  sql <- sprintf(
    "SELECT owner, COUNT(*) AS segments, SUM(bytes) AS size_bytes,
            ROUND(100 * RATIO_TO_REPORT(SUM(bytes)) OVER (), 2) AS pct
       FROM dba_segments
      WHERE %s
      GROUP BY owner
      ORDER BY size_bytes DESC",
    filter
  )
  res <- ora_query(con, sql, object = "DBA_SEGMENTS")
  ora_apply_unit(res, "size_bytes", unit)
}

#' Size per segment type of one schema
#'
#' Reads `DBA_SEGMENTS`.
#'
#' @inheritParams ora_schema_sizes
#' @param owner the schema name.
#' @return a data.frame, largest first, with columns `segment_type`,
#'   `segments`, `size_<unit>` and `pct` (of the schema).
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_schema_breakdown(con, Sys.getenv("ORA_USER"))
#' DBI::dbDisconnect(con)
ora_schema_breakdown <- function(con, owner, unit = "GB") {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  checkmate::assert_choice(unit, names(ora_units))
  sql <- "
    SELECT segment_type, COUNT(*) AS segments, SUM(bytes) AS size_bytes,
           ROUND(100 * RATIO_TO_REPORT(SUM(bytes)) OVER (), 2) AS pct
      FROM dba_segments
     WHERE owner = ?
     GROUP BY segment_type
     ORDER BY size_bytes DESC"
  res <- ora_query(con, sql, params = list(owner), object = "DBA_SEGMENTS")
  ora_apply_unit(res, "size_bytes", unit)
}

#' Size per table including partitions, indexes and LOBs
#'
#' IOT data lives in the `IOT - TOP` index and is counted as data. Reads
#' `DBA_SEGMENTS` with `DBA_INDEXES` and `DBA_LOBS`, falling back to
#' `ALL_INDEXES` and `ALL_LOBS` (objects the user cannot see are missed).
#'
#' @inheritParams ora_schema_breakdown
#' @return a data.frame, largest first, with columns `table_name`, `iot`,
#'   `data_segments`, `data_<unit>`, `index_<unit>`, `lob_<unit>` and
#'   `total_<unit>`. Attribute `partial` is `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_table_sizes(con, Sys.getenv("ORA_USER"))
#' DBI::dbDisconnect(con)
ora_table_sizes <- function(con, owner, unit = "GB") {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  checkmate::assert_choice(unit, names(ora_units))
  template <- "
    WITH seg AS (
      SELECT segment_name, segment_type, bytes
        FROM dba_segments
       WHERE owner = ?
    ), tab AS (
      SELECT segment_name AS table_name, COUNT(*) AS segments,
             SUM(bytes) AS bytes
        FROM seg
       WHERE segment_type IN ('TABLE', 'TABLE PARTITION',
                              'TABLE SUBPARTITION')
       GROUP BY segment_name
    ), idx AS (
      SELECT i.table_name,
             SUM(CASE WHEN i.index_type = 'IOT - TOP' THEN 1 ELSE 0 END)
               AS iot_segments,
             SUM(CASE WHEN i.index_type = 'IOT - TOP' THEN s.bytes ELSE 0 END)
               AS iot_bytes,
             SUM(CASE WHEN i.index_type = 'IOT - TOP' THEN 0 ELSE s.bytes END)
               AS index_bytes
        FROM seg s
        JOIN {dict}_indexes i
          ON i.owner = ? AND i.index_name = s.segment_name
       WHERE s.segment_type IN ('INDEX', 'INDEX PARTITION',
                                'INDEX SUBPARTITION')
         AND i.index_type <> 'LOB'
       GROUP BY i.table_name
    ), lob AS (
      SELECT l.table_name, SUM(s.bytes) AS bytes
        FROM seg s
        JOIN {dict}_lobs l
          ON l.owner = ? AND s.segment_name IN (l.segment_name, l.index_name)
       WHERE s.segment_type NOT IN ('TABLE', 'TABLE PARTITION',
                                    'TABLE SUBPARTITION')
       GROUP BY l.table_name
    ), tabs AS (
      SELECT table_name FROM tab
      UNION SELECT table_name FROM idx
      UNION SELECT table_name FROM lob
    )
    SELECT n.table_name,
           CASE WHEN NVL(x.iot_segments, 0) > 0 THEN 'Y' ELSE 'N' END AS iot,
           NVL(t.segments, 0) + NVL(x.iot_segments, 0) AS data_segments,
           NVL(t.bytes, 0) + NVL(x.iot_bytes, 0) AS data_bytes,
           NVL(x.index_bytes, 0) AS index_bytes,
           NVL(l.bytes, 0) AS lob_bytes,
           NVL(t.bytes, 0) + NVL(x.iot_bytes, 0) + NVL(x.index_bytes, 0)
             + NVL(l.bytes, 0) AS total_bytes
      FROM tabs n
      LEFT JOIN tab t ON t.table_name = n.table_name
      LEFT JOIN idx x ON x.table_name = n.table_name
      LEFT JOIN lob l ON l.table_name = n.table_name
     ORDER BY total_bytes DESC"
  res <- ora_query_dict(
    con,
    template,
    c("INDEXES", "LOBS"),
    params = list(owner, owner, owner)
  )
  res$iot <- ora_yes_no(res$iot)
  ora_apply_unit(
    res,
    c("data_bytes", "index_bytes", "lob_bytes", "total_bytes"),
    unit
  )
}

#' Size per index
#'
#' LOB indexes are left out, see [ora_lob_sizes()]. Reads `DBA_SEGMENTS`
#' with `DBA_INDEXES`, falling back to `ALL_INDEXES`.
#'
#' @inheritParams ora_schema_breakdown
#' @return a data.frame, largest first, with columns `index_name`,
#'   `table_name`, `index_type`, `iot_top`, `uniqueness`, `partitioned`,
#'   `segments` and `size_<unit>`. Attribute `partial` is `TRUE` after a
#'   fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_index_sizes(con, Sys.getenv("ORA_USER"))
#' DBI::dbDisconnect(con)
ora_index_sizes <- function(con, owner, unit = "GB") {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  checkmate::assert_choice(unit, names(ora_units))
  template <- "
    SELECT i.index_name, i.table_name, i.index_type,
           CASE WHEN i.index_type = 'IOT - TOP' THEN 'Y' ELSE 'N' END
             AS iot_top,
           i.uniqueness, i.partitioned,
           COUNT(*) AS segments, SUM(s.bytes) AS size_bytes
      FROM dba_segments s
      JOIN {dict}_indexes i
        ON i.owner = s.owner AND i.index_name = s.segment_name
     WHERE s.owner = ?
       AND s.segment_type IN ('INDEX', 'INDEX PARTITION',
                              'INDEX SUBPARTITION')
       AND i.index_type <> 'LOB'
     GROUP BY i.index_name, i.table_name, i.index_type, i.uniqueness,
              i.partitioned
     ORDER BY size_bytes DESC"
  res <- ora_query_dict(con, template, "INDEXES", params = list(owner))
  res$iot_top <- ora_yes_no(res$iot_top)
  res$partitioned <- ora_yes_no(res$partitioned)
  ora_apply_unit(res, "size_bytes", unit)
}

#' Size per LOB column
#'
#' Counts LOB segment and LOB index. Reads `DBA_SEGMENTS` with `DBA_LOBS`,
#' falling back to `ALL_LOBS`.
#'
#' @inheritParams ora_schema_breakdown
#' @return a data.frame, largest first, with columns `table_name`,
#'   `column_name`, `segment_name`, `securefile`, `partitioned`, `segments`
#'   and `size_<unit>`. Attribute `partial` is `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_lob_sizes(con, Sys.getenv("ORA_USER"))
#' DBI::dbDisconnect(con)
ora_lob_sizes <- function(con, owner, unit = "GB") {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  checkmate::assert_choice(unit, names(ora_units))
  template <- "
    SELECT l.table_name, l.column_name, l.segment_name, l.securefile,
           l.partitioned, COUNT(*) AS segments, SUM(s.bytes) AS size_bytes
      FROM dba_segments s
      JOIN {dict}_lobs l
        ON l.owner = s.owner
       AND s.segment_name IN (l.segment_name, l.index_name)
     WHERE s.owner = ?
       AND s.segment_type NOT IN ('TABLE', 'TABLE PARTITION',
                                  'TABLE SUBPARTITION')
     GROUP BY l.table_name, l.column_name, l.segment_name, l.securefile,
              l.partitioned
     ORDER BY size_bytes DESC"
  res <- ora_query_dict(con, template, "LOBS", params = list(owner))
  res$securefile <- ora_yes_no(res$securefile)
  res$partitioned <- ora_yes_no(res$partitioned)
  ora_apply_unit(res, "size_bytes", unit)
}

#' Size of the recycle bin per schema
#'
#' Counts `BIN$` segments in `DBA_SEGMENTS`.
#'
#' @inheritParams ora_schema_sizes
#' @param owner the schema name, or `NULL` for all schemas.
#' @return a data.frame, largest first, with columns `owner`, `segments`
#'   and `size_<unit>`.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_recyclebin_size(con)
#' DBI::dbDisconnect(con)
ora_recyclebin_size <- function(
  con,
  owner = NULL,
  unit = "GB",
  include_oracle = FALSE
) {
  assert_dbi_con(con)
  checkmate::assert_choice(unit, names(ora_units))
  checkmate::assert_flag(include_oracle)
  params <- NULL
  if (!is.null(owner)) {
    params <- list(ora_as_name(owner, len = 1L))
    filter <- "owner = ?"
  } else if (include_oracle) {
    filter <- "1 = 1"
  } else {
    filter <- ora_owner_filter("owner")
  }
  sql <- sprintf(
    "SELECT owner, COUNT(*) AS segments, SUM(bytes) AS size_bytes
       FROM dba_segments
      WHERE segment_name LIKE 'BIN$%%'
        AND %s
      GROUP BY owner
      ORDER BY size_bytes DESC",
    filter
  )
  res <- ora_query(con, sql, params = params, object = "DBA_SEGMENTS")
  ora_apply_unit(res, "size_bytes", unit)
}
