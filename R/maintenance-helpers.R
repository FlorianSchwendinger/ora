#' Look up one index
#'
#' @param con a DBI connection.
#' @param owner the uppercased schema name.
#' @param index the uppercased index name.
#' @return a one-row data.frame with columns `index_name`, `index_type`,
#'   `table_name` and `partitioned` (logical).
#' @noRd
ora_index_info <- function(con, owner, index) {
  template <- "
    SELECT index_name, index_type, table_name, partitioned
      FROM {dict}_indexes
     WHERE owner = ? AND index_name = ?"
  res <- ora_query_dict(con, template, "INDEXES", params = list(owner, index))
  if (nrow(res) == 0L) {
    stop(
      sprintf("Index %s.%s not found or not visible.", owner, index),
      call. = FALSE
    )
  }
  res$partitioned <- ora_yes_no(res$partitioned)
  res
}

#' Test whether an index is an `IOT - TOP` index
#'
#' @param con a DBI connection.
#' @param owner the uppercased schema name.
#' @param index the uppercased index name.
#' @return `TRUE` or `FALSE`.
#' @noRd
ora_is_iot_top <- function(con, owner, index) {
  identical(ora_index_info(con, owner, index)$index_type, "IOT - TOP")
}

#' Look up one table
#'
#' @param con a DBI connection.
#' @param owner the uppercased schema name.
#' @param table the uppercased table name.
#' @return a one-row data.frame with columns `table_name`, `partitioned`
#'   (logical), `iot` (logical), `row_movement` (logical) and `iot_index`
#'   (the `IOT - TOP` index name or `NA`).
#' @noRd
ora_table_info <- function(con, owner, table) {
  template <- "
    SELECT t.table_name, t.partitioned, t.iot_type, t.row_movement,
           (SELECT i.index_name
              FROM {dict}_indexes i
             WHERE i.owner = t.owner
               AND i.table_name = t.table_name
               AND i.index_type = 'IOT - TOP') AS iot_index
      FROM {dict}_tables t
     WHERE t.owner = ? AND t.table_name = ?"
  res <- ora_query_dict(
    con,
    template,
    c("TABLES", "INDEXES"),
    params = list(owner, table)
  )
  if (nrow(res) == 0L) {
    stop(
      sprintf("Table %s.%s not found or not visible.", owner, table),
      call. = FALSE
    )
  }
  data.frame(
    table_name = res$table_name,
    partitioned = ora_yes_no(res$partitioned),
    iot = res$iot_type %in% "IOT",
    row_movement = res$row_movement %in% "ENABLED",
    iot_index = res$iot_index
  )
}

#' Partition names of a table, by position
#'
#' `HIGH_VALUE` is a `LONG` column and is not read.
#'
#' @param con a DBI connection.
#' @param owner the uppercased schema name.
#' @param table the uppercased table name.
#' @return a character vector of partition names.
#' @noRd
ora_table_partitions <- function(con, owner, table) {
  template <- "
    SELECT partition_name
      FROM {dict}_tab_partitions
     WHERE table_owner = ? AND table_name = ?
     ORDER BY partition_position"
  res <- ora_query_dict(
    con,
    template,
    "TAB_PARTITIONS",
    params = list(owner, table)
  )
  res$partition_name
}

#' Partition names of an index, by position
#'
#' @param con a DBI connection.
#' @param owner the uppercased schema name.
#' @param index the uppercased index name.
#' @return a character vector of partition names.
#' @noRd
ora_index_partitions <- function(con, owner, index) {
  template <- "
    SELECT partition_name
      FROM {dict}_ind_partitions
     WHERE index_owner = ? AND index_name = ?
     ORDER BY partition_position"
  res <- ora_query_dict(
    con,
    template,
    "IND_PARTITIONS",
    params = list(owner, index)
  )
  res$partition_name
}

#' Check requested partitions against existing ones
#'
#' @param requested a character vector of partition names, or `NULL` for
#'   all.
#' @param existing a character vector of existing partition names.
#' @param object the table or index name, used in error messages.
#' @return the uppercased requested partitions, or `existing`.
#' @noRd
ora_match_partitions <- function(requested, existing, object) {
  checkmate::assert_character(existing, any.missing = FALSE)
  checkmate::assert_string(object)
  if (is.null(requested)) {
    return(existing)
  }
  requested <- ora_as_name(requested, .var.name = "partitions")
  unknown <- setdiff(requested, existing)
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "Unknown partitions of %s: %s",
        object,
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  unique(requested)
}

#' Current size of a segment or segment partition
#'
#' @param con a DBI connection.
#' @param owner the uppercased schema name.
#' @param segment the segment name, or `NA`.
#' @param partition the partition name, or `NA` for all partitions.
#' @return the size in bytes, `0` if no segment exists, or `NA` if
#'   `segment` is `NA`.
#' @noRd
ora_segment_bytes <- function(con, owner, segment, partition = NA) {
  if (is.na(segment)) {
    return(NA_real_)
  }
  sql <- "SELECT NVL(SUM(bytes), 0) AS bytes FROM dba_segments
           WHERE owner = ? AND segment_name = ?"
  params <- list(owner, segment)
  if (!is.na(partition)) {
    sql <- paste(sql, "AND partition_name = ?")
    params <- c(params, list(partition))
  }
  res <- ora_query(con, sql, params = params, object = "DBA_SEGMENTS")
  as.numeric(res$bytes)
}

#' Build an `ora_sql` object
#'
#' @param sql a character vector of statements.
#' @param owner the schema name.
#' @param table_name the table name, or `NA`.
#' @param segment_name the segment measured before and after, or `NA`.
#' @param partition_name the partition names, or `NA`.
#' @return a data.frame of class `ora_sql` with one row per statement.
#' @noRd
ora_sql_new <- function(
  sql,
  owner = NA_character_,
  table_name = NA_character_,
  segment_name = NA_character_,
  partition_name = NA_character_
) {
  sql <- as.character(sql)
  checkmate::assert_character(sql, min.len = 1L, any.missing = FALSE)
  for (s in sql) {
    ora_check_sql(s)
  }
  res <- data.frame(
    sql = sql,
    owner = as.character(owner),
    table_name = as.character(table_name),
    segment_name = as.character(segment_name),
    partition_name = as.character(partition_name)
  )
  class(res) <- c("ora_sql", "data.frame")
  res
}

#' Convert SQL text or an `ora_sql` object to `ora_sql`
#'
#' @param x a character vector or an `ora_sql` object.
#' @return an `ora_sql` object.
#' @noRd
ora_as_sql <- function(x) {
  if (inherits(x, "ora_sql")) {
    return(x)
  }
  ora_sql_new(x)
}

#' Combine `ora_sql` objects
#'
#' @param ... `ora_sql` objects.
#' @return an `ora_sql` object.
#' @noRd
ora_sql_bind <- function(...) {
  parts <- list(...)
  res <- do.call(rbind, lapply(parts, as.data.frame))
  class(res) <- c("ora_sql", "data.frame")
  res
}

#' Print generated SQL
#'
#' Prints an SQL comment saying the statements were not run, then the
#' statements.
#'
#' @param x an `ora_sql` object.
#' @param ... ignored.
#' @return `x`, invisibly.
#' @export
#' @keywords internal
print.ora_sql <- function(x, ...) {
  cat(
    "-- Generated SQL, NOT executed. Inspect it, then run it with",
    "-- ora_run_sql(con, sql, confirm = TRUE).",
    x$sql,
    sep = "\n"
  )
  invisible(x)
}

#' Generated SQL as text
#'
#' @param x an `ora_sql` object.
#' @param ... ignored.
#' @return a character vector of statements.
#' @export
#' @keywords internal
as.character.ora_sql <- function(x, ...) {
  x$sql
}
