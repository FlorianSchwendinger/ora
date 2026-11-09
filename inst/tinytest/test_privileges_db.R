source("helper_db.R", local = TRUE)
con <- ora_test_con()
if (is.null(con)) {
  exit_file("ORA_TEST_DSN not set")
}
me <- toupper(Sys.getenv("ORA_TEST_USER"))

# ora_user_exists (ALL_USERS, always readable)
expect_equal(
  ora::ora_user_exists(con, c(me, "NO_SUCH_USER_XYZ")),
  stats::setNames(c(TRUE, FALSE), c(me, "NO_SUCH_USER_XYZ"))
)

# ora_object_owner (falls back to ALL_OBJECTS)
res <- suppressMessages(ora::ora_object_owner(con, "dual"))
expect_true("SYS" %in% res$owner)
expect_equal(names(res), c("owner", "object_name", "object_type", "status"))
expect_true(is.logical(attr(res, "partial")))

# ora_database_vault_enabled
dv <- suppressMessages(ora::ora_database_vault_enabled(con))
expect_true(isTRUE(dv) || isFALSE(as.vector(dv)))

# Grant trees need DBA_SYS_PRIVS / DBA_ROLE_PRIVS
if (all(ora::ora_can_read(con, c("DBA_SYS_PRIVS", "DBA_ROLE_PRIVS")))) {
  res <- ora::ora_who_has_privilege(con, "create session")
  expect_equal(
    names(res),
    c("privilege", "grantee", "grantee_type", "direct", "via")
  )
  expect_true(me %in% res$grantee)
  expect_true(is.logical(res$direct))
  expect_false("SYS" %in% res$grantee)
  res <- ora::ora_who_has_privilege(
    con,
    "CREATE SESSION",
    include_oracle = TRUE
  )
  expect_true("SYS" %in% res$grantee)
  res <- ora::ora_who_has_role(con, "DBA", include_oracle = TRUE)
  expect_true("SYSTEM" %in% res$grantee)
} else {
  expect_error(
    ora::ora_who_has_privilege(con, "CREATE SESSION"),
    class = "ora_no_privilege"
  )
}

# ora_schema_sessions needs V$SESSION
if (ora::ora_can_read(con, "V$SESSION")) {
  res <- ora::ora_schema_sessions(con, me)
  expect_true(nrow(res) >= 1L)
} else {
  expect_error(ora::ora_schema_sessions(con, me), class = "ora_no_privilege")
}

DBI::dbDisconnect(con)
