source("helper_db.R", local = TRUE)
con <- ora_test_con()
if (is.null(con)) {
  exit_file("ORA_TEST_DSN not set")
}
me <- toupper(Sys.getenv("ORA_TEST_USER"))

# ora_require_views
expect_true(ora:::ora_require_views(con, "DUAL"))
expect_error(
  ora:::ora_require_views(con, c("DUAL", "NO_SUCH_VIEW_XYZ")),
  class = "ora_no_privilege"
)
err <- tryCatch(
  ora:::ora_require_views(con, "NO_SUCH_VIEW_XYZ"),
  ora_no_privilege = function(e) e
)
expect_equal(err$object, "NO_SUCH_VIEW_XYZ")

# ora_instance_uptime
if (ora::ora_can_read(con, "V$INSTANCE")) {
  up <- ora::ora_instance_uptime(con)
  expect_equal(nrow(up), 1L)
  expect_true(up$uptime_days >= 0)
} else {
  expect_error(ora::ora_instance_uptime(con), class = "ora_no_privilege")
}

if (!ora::ora_can_read(con, "DBA_SEGMENTS")) {
  DBI::dbDisconnect(con)
  exit_file("DBA_SEGMENTS not readable")
}

for (t in c("ORA_T_HOT", "ORA_T_IDX")) {
  try(ora:::ora_execute(con, sprintf("DROP TABLE %s PURGE", t)), silent = TRUE)
}
ora:::ora_execute(con, "CREATE TABLE ora_t_hot (id NUMBER)")
ora:::ora_execute(con, "INSERT INTO ora_t_hot VALUES (1)")
ora:::ora_execute(
  con,
  "CREATE TABLE ora_t_idx (id NUMBER PRIMARY KEY, v NUMBER)"
)
ora:::ora_execute(con, "INSERT INTO ora_t_idx VALUES (1, 1)")
ora:::ora_execute(con, "CREATE INDEX ora_t_idx_v ON ora_t_idx (v)")
ora:::ora_execute(con, "COMMIT")
for (i in 1:5) {
  ora:::ora_query(con, "SELECT COUNT(*) AS n FROM ora_t_hot")
}

# ora_untouched_tables
if (ora::ora_can_read(con, "V$SEGMENT_STATISTICS")) {
  ut <- suppressMessages(ora::ora_untouched_tables(con, me))
  expect_equal(
    names(ut),
    c("table_name", "segments", "size_gb", "logical_reads", "block_changes")
  )
  expect_true(all(ut$logical_reads == 0))
  expect_false("ORA_T_HOT" %in% ut$table_name)
} else {
  expect_error(
    ora::ora_untouched_tables(con, me),
    class = "ora_no_privilege"
  )
}

# ora_unused_indexes
if (ora::ora_can_read(con, "DBA_INDEX_USAGE")) {
  ui <- suppressMessages(ora::ora_unused_indexes(con, me))
  expect_true("ORA_T_IDX_V" %in% ui$index_name)
  pk <- ora:::ora_query(
    con,
    "SELECT index_name FROM user_constraints
      WHERE table_name = 'ORA_T_IDX' AND constraint_type = 'P'"
  )
  expect_false(pk$index_name %in% ui$index_name)
} else {
  err <- tryCatch(
    ora::ora_unused_indexes(con, me),
    ora_no_privilege = function(e) e
  )
  expect_equal(err$object, "DBA_INDEX_USAGE")
}

# ora_table_modifications
flushed <- tryCatch(
  {
    ora:::ora_execute(con, "CALL dbms_stats.flush_database_monitoring_info()")
    TRUE
  },
  error = function(e) FALSE
)
tm <- suppressMessages(ora::ora_table_modifications(con, me, "ora_t_hot"))
expect_true(all(c("inserts", "pct_changed", "timestamp") %in% names(tm)))
expect_true(is.logical(tm$truncated))
if (flushed) {
  expect_true(any(tm$inserts >= 1))
}

ora:::ora_execute(con, "DROP TABLE ora_t_hot PURGE")
ora:::ora_execute(con, "DROP TABLE ora_t_idx PURGE")
DBI::dbDisconnect(con)
