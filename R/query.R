#' Stop if SQL ends with a semicolon
#'
#' Oracle drivers reject a trailing semicolon with ORA-00911.
#'
#' @param sql an SQL string.
#' @return `sql`, invisibly.
#' @noRd
ora_check_sql <- function(sql) {
  checkmate::assert_string(sql, min.chars = 1L)
  if (grepl(";[[:space:]]*$", sql)) {
    stop("SQL must not end with a semicolon (ORA-00911).", call. = FALSE)
  }
  invisible(sql)
}

#' Run a query with bind parameters
#'
#' Privilege errors become `ora_no_privilege` conditions. Column names are
#' lowercased.
#'
#' @param con a DBI connection.
#' @param sql an SQL query with `?` placeholders.
#' @param params a list of values for the placeholders, or `NULL`.
#' @param object the view or object queried, used in error messages.
#' @return a data.frame.
#' @noRd
ora_query <- function(con, sql, params = NULL, object = NA_character_) {
  assert_dbi_con(con)
  ora_check_sql(sql)
  checkmate::assert_list(params, null.ok = TRUE)
  checkmate::assert_string(object, na.ok = TRUE)
  res <- tryCatch(
    if (length(params) == 0L) {
      DBI::dbGetQuery(con, sql)
    } else {
      DBI::dbGetQuery(con, sql, params = params)
    },
    error = function(e) stop(ora_as_condition(e, object))
  )
  ora_normalize_names(res)
}

#' Run a statement that returns no rows
#'
#' Privilege errors become `ora_no_privilege` conditions.
#'
#' @param con a DBI connection.
#' @param sql an SQL or DDL statement with `?` placeholders.
#' @param params a list of values for the placeholders, or `NULL`.
#' @param object the object affected, used in error messages.
#' @return the number of affected rows, invisibly.
#' @noRd
ora_execute <- function(con, sql, params = NULL, object = NA_character_) {
  assert_dbi_con(con)
  ora_check_sql(sql)
  checkmate::assert_list(params, null.ok = TRUE)
  checkmate::assert_string(object, na.ok = TRUE)
  n <- tryCatch(
    if (length(params) == 0L) {
      DBI::dbExecute(con, sql)
    } else {
      DBI::dbExecute(con, sql, params = params)
    },
    error = function(e) stop(ora_as_condition(e, object))
  )
  invisible(n)
}

#' Run a query, falling back to a second query on missing privileges
#'
#' If `sql` raises `ora_no_privilege`, `fallback_sql` is run instead, an
#' `ora_partial` message is signalled and attribute `partial` is `TRUE`.
#'
#' @param con a DBI connection.
#' @param sql the preferred query, usually on a `DBA_*` view.
#' @param fallback_sql the query on a readable view, with the same columns
#'   as `sql`.
#' @param object the view used by `sql`.
#' @param fallback the view used by `fallback_sql`.
#' @param params a list of values for the placeholders of both queries.
#' @return a data.frame with attribute `partial`.
#' @noRd
ora_query_fallback <- function(
  con,
  sql,
  fallback_sql,
  object,
  fallback,
  params = NULL
) {
  checkmate::assert_string(object)
  checkmate::assert_string(fallback)
  ora_check_sql(fallback_sql)
  res <- tryCatch(
    ora_query(con, sql, params, object),
    ora_no_privilege = function(e) NULL
  )
  partial <- is.null(res)
  if (partial) {
    message(ora_partial(object, fallback))
    res <- ora_query(con, fallback_sql, params, fallback)
  }
  attr(res, "partial") <- partial
  res
}

#' Fill the dictionary prefix of a SQL template
#'
#' @param template an SQL template with `{dict}` placeholders, e.g.
#'   `{dict}_indexes`.
#' @param dict `"dba"` or `"all"`.
#' @return an SQL string.
#' @noRd
ora_render_dict <- function(template, dict) {
  checkmate::assert_string(template)
  checkmate::assert_choice(dict, c("dba", "all"))
  as.character(glue::glue(template, dict = dict))
}

#' Run a dictionary query on `DBA_*` views, falling back to `ALL_*`
#'
#' @param con a DBI connection.
#' @param template an SQL template with `{dict}` placeholders.
#' @param views the views behind `{dict}`, e.g. `c("INDEXES", "LOBS")`.
#' @param params a list of bind values, or `NULL`.
#' @return a data.frame with attribute `partial`.
#' @noRd
ora_query_dict <- function(con, template, views, params = NULL) {
  checkmate::assert_character(views, min.len = 1L, any.missing = FALSE)
  ora_query_fallback(
    con,
    sql = ora_render_dict(template, "dba"),
    fallback_sql = ora_render_dict(template, "all"),
    object = paste0("DBA_", views, collapse = ", "),
    fallback = paste0("ALL_", views, collapse = ", "),
    params = params
  )
}

#' Stop unless all views are readable
#'
#' Use for views without an `ALL_*` fallback, so the error names the right
#' view.
#'
#' @param con a DBI connection.
#' @param views a character vector of view names.
#' @return `TRUE`, invisibly, or an `ora_no_privilege` error.
#' @noRd
ora_require_views <- function(con, views) {
  ok <- ora_can_read(con, views)
  if (!all(ok)) {
    stop(ora_no_privilege(names(ok)[!ok][1L], "ORA-00942"))
  }
  invisible(TRUE)
}
