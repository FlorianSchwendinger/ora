# Runs as the low-privilege user that mirrors production, against the
# fixture schema SPACEAPP from inst/test-setup/02_fixture.sql.
source("helper_db.R", local = TRUE)
con <- ora_test_con("lowpriv")
if (is.null(con)) {
  exit_file("ORA_TEST_LOWPRIV_USER not set")
}
owner <- "SPACEAPP"

# What production can and cannot read
r <- ora::ora_can_read(
  con,
  c(
    "DBA_SEGMENTS",
    "DBA_TABLESPACE_USAGE_METRICS",
    "DBA_USERS",
    "V$SESSION",
    "DBA_INDEXES",
    "ALL_INDEXES"
  )
)
expect_equal(
  unname(r),
  c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)
)

# Works with direct grants
expect_equal(nrow(ora::ora_version(con)), 1L)
expect_equal(nrow(ora::ora_db_fullness(con)), 1L)
expect_true(nrow(ora::ora_tablespace_usage(con)) >= 1L)
expect_true(owner %in% ora::ora_schema_sizes(con)$owner)
expect_true(ora::ora_user_exists(con, owner))
expect_true(nrow(ora::ora_who_has_privilege(con, "CREATE SESSION")) >= 1L)

# ALL_* fallbacks: partial results with a message
expect_message(ora::ora_object_owner(con, "SALES"), class = "ora_partial")
oo <- suppressMessages(ora::ora_object_owner(con, "SALES"))
expect_true(attr(oo, "partial"))
expect_true(owner %in% oo$owner)

ts <- suppressMessages(ora::ora_table_sizes(con, owner))
expect_true(attr(ts, "partial"))
expect_true(ts$iot[ts$table_name == "MEASUREMENTS"])

iot <- suppressMessages(ora::ora_iot_tables(con, owner))
expect_equal(
  iot$index_name[iot$table_name == "MEASUREMENTS"],
  "MEASUREMENTS_PK"
)
ps <- suppressMessages(ora::ora_partition_sizes(con, owner, "MEASUREMENTS"))
expect_true(nrow(ps) >= 36L)

est <- suppressMessages(ora::ora_estimate_data_size(con, owner, "SALES"))
expect_true(est$stats_locked)
expect_false(est$reliable)

# No fallback: classed errors naming the view
err <- tryCatch(
  ora::ora_schema_sessions(con, owner),
  ora_no_privilege = function(e) e
)
expect_equal(err$object, "V$SESSION")
err <- tryCatch(
  ora::ora_unused_indexes(con, owner),
  ora_no_privilege = function(e) e
)
expect_equal(err$object, "DBA_INDEX_USAGE")
expect_error(ora::ora_fra_usage(con), class = "ora_no_privilege")
expect_error(ora::ora_instance_uptime(con), class = "ora_no_privilege")

# SQL is generated, running it fails without ALTER privileges
sql <- suppressMessages(
  ora::ora_sql_rebuild_index(con, owner, "MEASUREMENTS_PK", "P2023_01")
)
expect_true(grepl("MOVE PARTITION \"P2023_01\"$", sql$sql))
expect_error(
  suppressMessages(ora::ora_run_sql(con, sql, confirm = TRUE)),
  "check_fra = FALSE"
)
err <- tryCatch(
  suppressMessages(
    ora::ora_run_sql(con, sql, confirm = TRUE, check_fra = FALSE)
  ),
  ora_run_error = function(e) e
)
expect_inherits(err, "ora_run_error")
expect_true(grepl("ORA-01031", conditionMessage(err)))

DBI::dbDisconnect(con)
