#' SQL that follows grants through nested roles
#'
#' @param anchor an SQL query returning `granted`, `grantee`, `via` (NULL)
#'   and `lvl` (1).
#' @param include_oracle a flag, keep Oracle-maintained users.
#' @return an SQL string.
#' @noRd
ora_grant_tree_sql <- function(anchor, include_oracle = FALSE) {
  checkmate::assert_string(anchor)
  checkmate::assert_flag(include_oracle)
  filter <- if (include_oracle) {
    "1 = 1"
  } else {
    "(u.username IS NULL OR u.oracle_maintained = 'N')"
  }
  sql <- glue::glue(
    "
    WITH tree (granted, grantee, via, lvl) AS (
      {anchor}
      UNION ALL
      SELECT t.granted, rp.grantee,
             CASE WHEN t.via IS NULL THEN t.grantee
                  ELSE t.via || ' > ' || t.grantee END,
             t.lvl + 1
        FROM tree t
        JOIN dba_role_privs rp ON rp.granted_role = t.grantee
    )
    SELECT t.granted, t.grantee,
           CASE WHEN t.grantee = 'PUBLIC' THEN 'PUBLIC'
                WHEN u.username IS NULL THEN 'ROLE'
                ELSE 'USER' END AS grantee_type,
           CASE WHEN t.lvl = 1 THEN 'Y' ELSE 'N' END AS direct,
           t.via
      FROM tree t
      LEFT JOIN all_users u ON u.username = t.grantee
     WHERE {filter}
     ORDER BY t.granted, t.grantee, t.lvl"
  )
  as.character(sql)
}

#' Run a grant tree query and tidy the result
#'
#' @param con a DBI connection.
#' @param anchor the anchor SQL for `ora_grant_tree_sql()`.
#' @param params a list of bind values for the anchor.
#' @param object the view used by the anchor.
#' @param granted_col the name for the `granted` column.
#' @param include_oracle a flag, keep Oracle-maintained users.
#' @return a data.frame.
#' @noRd
ora_grant_tree <- function(
  con,
  anchor,
  params,
  object,
  granted_col,
  include_oracle
) {
  sql <- ora_grant_tree_sql(anchor, include_oracle)
  res <- ora_query(con, sql, params = params, object = object)
  res$direct <- res$direct == "Y"
  names(res)[names(res) == "granted"] <- granted_col
  res
}

#' Users and roles that hold a system privilege
#'
#' Follows grants through nested roles. Reads `DBA_SYS_PRIVS`,
#' `DBA_ROLE_PRIVS` and `ALL_USERS`.
#'
#' @inheritParams ora_version
#' @param privilege a character vector of system privileges, e.g.
#'   `"ALTER ANY INDEX"`.
#' @param include_oracle a flag, keep Oracle-maintained users such as `SYS`.
#' @return a data.frame with columns `privilege`, `grantee`, `grantee_type`
#'   (`USER`, `ROLE` or `PUBLIC`), `direct` (logical) and `via` (role chain
#'   or `NA`).
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_who_has_privilege(con, "ALTER ANY INDEX")
#' DBI::dbDisconnect(con)
ora_who_has_privilege <- function(con, privilege, include_oracle = FALSE) {
  assert_dbi_con(con)
  privilege <- ora_as_name(privilege)
  anchor <- sprintf(
    "SELECT privilege, grantee, CAST(NULL AS VARCHAR2(4000)), 1
       FROM dba_sys_privs
      WHERE privilege IN (%s)",
    ora_placeholders(length(privilege))
  )
  ora_grant_tree(
    con,
    anchor,
    as.list(privilege),
    "DBA_SYS_PRIVS",
    "privilege",
    include_oracle
  )
}

#' Users and roles that hold a role
#'
#' Follows grants through nested roles. Reads `DBA_ROLE_PRIVS` and
#' `ALL_USERS`.
#'
#' @inheritParams ora_version
#' @param roles a character vector of role names.
#' @param include_oracle a flag, keep Oracle-maintained users such as `SYS`.
#' @return a data.frame with columns `role`, `grantee`, `grantee_type`
#'   (`USER`, `ROLE` or `PUBLIC`), `direct` (logical) and `via` (role chain
#'   or `NA`).
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_who_has_role(con, "DBA")
#' DBI::dbDisconnect(con)
ora_who_has_role <- function(con, roles, include_oracle = FALSE) {
  assert_dbi_con(con)
  roles <- ora_as_name(roles)
  anchor <- sprintf(
    "SELECT granted_role, grantee, CAST(NULL AS VARCHAR2(4000)), 1
       FROM dba_role_privs
      WHERE granted_role IN (%s)",
    ora_placeholders(length(roles))
  )
  ora_grant_tree(
    con,
    anchor,
    as.list(roles),
    "DBA_ROLE_PRIVS",
    "role",
    include_oracle
  )
}

#' Test whether users exist
#'
#' Reads `ALL_USERS`, so `DBA_USERS` is not needed.
#'
#' @inheritParams ora_version
#' @param username a character vector of user names.
#' @return a named logical vector, named by the uppercased user names.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_user_exists(con, c("SYSTEM", "NO_SUCH_USER"))
#' DBI::dbDisconnect(con)
ora_user_exists <- function(con, username) {
  assert_dbi_con(con)
  username <- ora_as_name(username)
  sql <- sprintf(
    "SELECT username FROM all_users WHERE username IN (%s)",
    ora_placeholders(length(username))
  )
  res <- ora_query(con, sql, params = as.list(username), object = "ALL_USERS")
  stats::setNames(username %in% res$username, username)
}

#' Owners of an object name
#'
#' Reads `DBA_OBJECTS`. Falls back to `ALL_OBJECTS`, which shows only
#' objects visible to the current user.
#'
#' @inheritParams ora_version
#' @param object_name a character vector of object names.
#' @return a data.frame with columns `owner`, `object_name`, `object_type`
#'   and `status`. Attribute `partial` is `TRUE` after a fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_object_owner(con, "DUAL")
#' DBI::dbDisconnect(con)
ora_object_owner <- function(con, object_name) {
  assert_dbi_con(con)
  object_name <- ora_as_name(object_name)
  template <- "
    SELECT owner, object_name, object_type, status
      FROM %s
     WHERE object_name IN (%s)
       AND subobject_name IS NULL
     ORDER BY object_name, owner, object_type"
  ph <- ora_placeholders(length(object_name))
  ora_query_fallback(
    con,
    sql = sprintf(template, "dba_objects", ph),
    fallback_sql = sprintf(template, "all_objects", ph),
    object = "DBA_OBJECTS",
    fallback = "ALL_OBJECTS",
    params = as.list(object_name)
  )
}

#' Sessions of a schema
#'
#' Reads `V$SESSION`. Without access this raises `ora_no_privilege`. The
#' DBA can grant `SELECT ON SYS.V_$SESSION`.
#'
#' @inheritParams ora_version
#' @param owner the schema name, matched against the session user and the
#'   current schema.
#' @return a data.frame with columns `sid`, `serial`, `username`,
#'   `schemaname`, `osuser`, `machine`, `program`, `status` and `logon_time`.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' try(ora_schema_sessions(con, Sys.getenv("ORA_USER")))
#' DBI::dbDisconnect(con)
ora_schema_sessions <- function(con, owner) {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  sql <- "
    SELECT sid, serial# AS serial, username, schemaname, osuser, machine,
           program, status, logon_time
      FROM v$session
     WHERE username = ? OR schemaname = ?
     ORDER BY logon_time"
  ora_query(con, sql, params = list(owner, owner), object = "V$SESSION")
}

#' Test whether Oracle Database Vault is enabled
#'
#' Reads `DBA_DV_STATUS`. Falls back to `V$OPTION`, which only shows
#' whether the option is installed.
#'
#' @inheritParams ora_version
#' @return `TRUE` or `FALSE`. Attribute `partial` is `TRUE` after a
#'   fallback.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' ora_database_vault_enabled(con)
#' DBI::dbDisconnect(con)
ora_database_vault_enabled <- function(con) {
  assert_dbi_con(con)
  res <- ora_query_fallback(
    con,
    sql = "SELECT status AS enabled FROM dba_dv_status
            WHERE name = 'DV_ENABLE_STATUS'",
    fallback_sql = "SELECT value AS enabled FROM v$option
                     WHERE parameter = 'Oracle Database Vault'",
    object = "DBA_DV_STATUS",
    fallback = "V$OPTION"
  )
  enabled <- nrow(res) > 0L && identical(toupper(res$enabled[1L]), "TRUE")
  attr(enabled, "partial") <- attr(res, "partial")
  enabled
}
