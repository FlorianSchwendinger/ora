#' Check whether examples can connect to Oracle
#'
#' Used by `@examplesIf`. Needs env var `ORA_DSN` and package odbc.
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @export
#' @examples
#' ora_has_example_db()
ora_has_example_db <- function() {
  nzchar(Sys.getenv("ORA_DSN")) && requireNamespace("odbc", quietly = TRUE)
}
