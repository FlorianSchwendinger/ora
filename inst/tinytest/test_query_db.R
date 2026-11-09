source("helper_db.R", local = TRUE)
con <- ora_test_con()
if (is.null(con)) {
  exit_file("ORA_TEST_DSN not set")
}

res <- ora:::ora_query(con, "SELECT 1 AS ONE FROM dual")
expect_equal(names(res), "one")

res <- ora:::ora_query(con, "SELECT ? AS X FROM dual", params = list("a"))
expect_equal(res$x, "a")

expect_error(
  ora:::ora_query(
    con,
    "SELECT 1 FROM no_such_view_xyz",
    object = "NO_SUCH_VIEW_XYZ"
  ),
  class = "ora_no_privilege"
)

DBI::dbDisconnect(con)
