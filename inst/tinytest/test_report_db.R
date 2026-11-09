source("helper_db.R", local = TRUE)
if (
  !requireNamespace("rmarkdown", quietly = TRUE) ||
    !rmarkdown::pandoc_available()
) {
  exit_file("rmarkdown or pandoc not available")
}
con <- ora_test_con()
if (is.null(con)) {
  exit_file("ORA_TEST_DSN not set")
}
me <- toupper(Sys.getenv("ORA_TEST_USER"))

# A partitioned table whose statistics are deleted and locked, so the
# automatic statistics job cannot analyse it.
try(ora:::ora_execute(con, "DROP TABLE ora_t_nostats PURGE"), silent = TRUE)
ora:::ora_execute(
  con,
  "CREATE TABLE ora_t_nostats (id NUMBER, d DATE)
   PARTITION BY RANGE (d) (
     PARTITION p1 VALUES LESS THAN (DATE '2024-01-01'),
     PARTITION p2 VALUES LESS THAN (MAXVALUE))"
)
ora:::ora_execute(
  con,
  "INSERT INTO ora_t_nostats SELECT level, DATE '2023-06-01' + level
     FROM dual CONNECT BY level <= 400"
)
ora:::ora_execute(con, "COMMIT")
ora:::ora_execute(
  con,
  "CALL dbms_stats.delete_table_stats(USER, 'ORA_T_NOSTATS')"
)
ora:::ora_execute(
  con,
  "CALL dbms_stats.lock_table_stats(USER, 'ORA_T_NOSTATS')"
)

# Full report as the main user, every section on
file <- tempfile(fileext = ".html")
out <- ora::ora_space_report(
  con,
  owner = me,
  tables = "ORA_T_NOSTATS",
  file = file,
  unit = "MB",
  show_activity = TRUE,
  show_cleanup = TRUE,
  open = FALSE
)
expect_true(file.exists(out))
expect_equal(normalizePath(out), normalizePath(file))
html <- gsub("\\s+", " ", paste(readLines(out, warn = FALSE), collapse = " "))
for (h in c(
  "Privileges",
  "Database fullness",
  "Schemas",
  "Biggest segments",
  "Index-organized tables",
  "Partitions",
  "Activity",
  "Cleanup SQL"
)) {
  expect_true(grepl(h, html, fixed = TRUE), info = h)
}
expect_true(grepl(
  "No partition statistics, no shrink suggested",
  html,
  fixed = TRUE
))
expect_true(grepl("Whole database, permanent and undo", html, fixed = TRUE))
expect_true(grepl("data:image/png", html, fixed = TRUE))
expect_false(grepl("Error in", html, fixed = TRUE))
ora:::ora_execute(con, "DROP TABLE ora_t_nostats PURGE")

# Sections switched off are left out
file2 <- tempfile(fileext = ".html")
ora::ora_space_report(
  con,
  file = file2,
  show_privileges = FALSE,
  show_schemas = FALSE,
  show_segments = FALSE,
  show_iot = FALSE,
  show_partitions = FALSE,
  open = FALSE
)
html2 <- paste(readLines(file2, warn = FALSE), collapse = "\n")
expect_true(grepl("Database fullness", html2, fixed = TRUE))
expect_false(grepl("Biggest segments", html2, fixed = TRUE))

expect_error(ora::ora_space_report(con, show_iot = NA, open = FALSE))
expect_error(
  ora::ora_space_report(con, file = "/no/such/dir/x.html", open = FALSE)
)
DBI::dbDisconnect(con)

# Low-privilege user: unreadable views are skipped, fallbacks are partial
con <- ora_test_con("lowpriv")
if (is.null(con)) {
  exit_file("ORA_TEST_LOWPRIV_USER not set")
}
file3 <- tempfile(fileext = ".html")
ora::ora_space_report(
  con,
  owner = "SPACEAPP",
  tables = c("SPACEAPP.MEASUREMENTS", "SPACEAPP.SALES"),
  file = file3,
  show_activity = TRUE,
  show_cleanup = TRUE,
  open = FALSE
)
html3 <- gsub(
  "\\s+",
  " ",
  paste(readLines(file3, warn = FALSE), collapse = " ")
)
expect_true(grepl("Skipped:", html3, fixed = TRUE))
expect_true(grepl("V$INSTANCE", html3, fixed = TRUE))
expect_true(grepl("Partial result", html3, fixed = TRUE))
expect_true(grepl("MEASUREMENTS_PK", html3, fixed = TRUE))
# SALES sits at its minimum size, MEASUREMENTS has reclaimable space
expect_true(grepl("shrinking frees nothing", html3, fixed = TRUE))
text3 <- gsub("&quot;", "\"", gsub("<[^>]+>", "", html3), fixed = TRUE)
expect_true(grepl("\"MEASUREMENTS\" MODIFY PARTITION", text3, fixed = TRUE))
expect_false(grepl("\"SALES\" MODIFY PARTITION", text3, fixed = TRUE))
expect_false(grepl("waste_pct.1", html3, fixed = TRUE))
expect_true(grepl("V$SESSION", html3, fixed = TRUE))
DBI::dbDisconnect(con)
