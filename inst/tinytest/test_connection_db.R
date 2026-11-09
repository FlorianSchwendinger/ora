source("helper_db.R", local = TRUE)
con <- ora_test_con()
if (is.null(con)) {
  exit_file("ORA_TEST_DSN not set")
}

# ora_version
v <- ora::ora_version(con)
expect_equal(nrow(v), 1L)
expect_equal(
  names(v),
  c("product", "edition", "version", "version_full", "status")
)
expect_true(grepl("^[0-9]+[.]", v$version_full))
expect_equal(v$product, trimws(v$product))

# ora_whoami
me <- ora::ora_whoami(con)
expect_equal(me$username, toupper(Sys.getenv("ORA_TEST_USER")))
expect_true(is.character(me$roles))
expect_true("CREATE SESSION" %in% me$privileges)

# ora_can_read
r <- ora::ora_can_read(con, c("dual", "no_such_view_xyz"))
expect_equal(r, c(DUAL = TRUE, NO_SUCH_VIEW_XYZ = FALSE))
expect_error(ora::ora_can_read(con, "dual where 1=1"), "Invalid Oracle name")

# ora_query_fallback
res <- ora:::ora_query_fallback(
  con,
  "SELECT username FROM no_such_view_xyz",
  "SELECT username FROM all_users WHERE ROWNUM = 1",
  object = "NO_SUCH_VIEW_XYZ",
  fallback = "ALL_USERS"
)
expect_true(attr(res, "partial"))
expect_message(
  ora:::ora_query_fallback(
    con,
    "SELECT 1 AS x FROM no_such_view_xyz",
    "SELECT 1 AS x FROM dual",
    object = "NO_SUCH_VIEW_XYZ",
    fallback = "DUAL"
  ),
  class = "ora_partial"
)
res <- ora:::ora_query_fallback(
  con,
  "SELECT 1 AS x FROM dual",
  "SELECT 2 AS x FROM dual",
  object = "DUAL",
  fallback = "DUAL"
)
expect_false(attr(res, "partial"))
expect_equal(res$x, 1)

DBI::dbDisconnect(con)
