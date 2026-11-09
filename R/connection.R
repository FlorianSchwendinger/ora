#' Database version
#'
#' Reads `PRODUCT_COMPONENT_VERSION`, which every user can read.
#'
#' @param con a DBI connection to Oracle, e.g. created from
#'   `DBI::dbConnect(odbc::odbc(), ...)`.
#' @return a one-row data.frame with columns `product`, `edition`,
#'   `version`, `version_full` and `status`.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_version(con)
#' DBI::dbDisconnect(con)
ora_version <- function(con) {
  sql <- "
    SELECT product, version, version_full, status
      FROM product_component_version
     WHERE product LIKE 'Oracle Database%'
     FETCH FIRST 1 ROWS ONLY"
  res <- ora_query(con, sql, object = "PRODUCT_COMPONENT_VERSION")
  res$product <- trimws(res$product)
  res$edition <- sub("^Oracle Database [0-9a-z]+ ", "", res$product)
  res[, c("product", "edition", "version", "version_full", "status")]
}

#' Current user, session roles and session privileges
#'
#' @inheritParams ora_version
#' @return a list with the strings `username`, `current_schema` and
#'   `container` and the character vectors `roles` and `privileges`.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_whoami(con)
#' DBI::dbDisconnect(con)
ora_whoami <- function(con) {
  sql <- "
    SELECT SYS_CONTEXT('USERENV', 'SESSION_USER') AS username,
           SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA') AS current_schema,
           SYS_CONTEXT('USERENV', 'CON_NAME') AS container
      FROM dual"
  user <- ora_query(con, sql, object = "DUAL")
  roles <- ora_query(
    con,
    "SELECT role FROM session_roles ORDER BY role",
    object = "SESSION_ROLES"
  )
  privs <- ora_query(
    con,
    "SELECT privilege FROM session_privs ORDER BY privilege",
    object = "SESSION_PRIVS"
  )
  list(
    username = user$username,
    current_schema = user$current_schema,
    container = user$container,
    roles = roles$role,
    privileges = privs$privilege
  )
}

#' Test which views the current user can read
#'
#' Runs `SELECT 1 FROM <view> WHERE ROWNUM = 1` for each view. A view that
#' does not exist also gives `FALSE`.
#'
#' @inheritParams ora_version
#' @param views a character vector of view names, optionally `OWNER.VIEW`.
#' @return a named logical vector, named by the uppercased views.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_can_read(con, c("DBA_SEGMENTS", "DBA_USERS", "V$SESSION"))
#' DBI::dbDisconnect(con)
ora_can_read <- function(con, views) {
  assert_dbi_con(con)
  views <- ora_as_name(views)
  quoted <- ora_quote_name(views)
  probe <- function(i) {
    sql <- sprintf("SELECT 1 FROM %s WHERE ROWNUM = 1", quoted[i])
    tryCatch(
      {
        ora_query(con, sql, object = views[i])
        TRUE
      },
      ora_no_privilege = function(e) FALSE
    )
  }
  res <- vapply(seq_along(views), probe, logical(1L))
  stats::setNames(res, views)
}
