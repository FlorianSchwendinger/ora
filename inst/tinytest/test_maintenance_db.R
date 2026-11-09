source("helper_db.R", local = TRUE)
con <- ora_test_con()
if (is.null(con)) {
  exit_file("ORA_TEST_DSN not set")
}
if (!ora::ora_can_read(con, "DBA_SEGMENTS")) {
  DBI::dbDisconnect(con)
  exit_file("DBA_SEGMENTS not readable")
}
me <- toupper(Sys.getenv("ORA_TEST_USER"))
fra_ok <- ora::ora_can_read(con, "V$RECOVERY_FILE_DEST")

for (t in c("ORA_T_HEAP", "ORA_T_MIOT")) {
  try(ora:::ora_execute(con, sprintf("DROP TABLE %s PURGE", t)), silent = TRUE)
}
ora:::ora_execute(
  con,
  "CREATE TABLE ora_t_heap (id NUMBER, d DATE, v VARCHAR2(200))
   PARTITION BY RANGE (d) (
     PARTITION p1 VALUES LESS THAN (DATE '2024-01-01'),
     PARTITION p2 VALUES LESS THAN (DATE '2025-01-01'),
     PARTITION p3 VALUES LESS THAN (MAXVALUE))"
)
ora:::ora_execute(con, "CREATE INDEX ora_t_heap_l ON ora_t_heap (d) LOCAL")
ora:::ora_execute(con, "CREATE INDEX ora_t_heap_g ON ora_t_heap (id)")
ora:::ora_execute(
  con,
  "INSERT INTO ora_t_heap
   SELECT level, DATE '2023-06-01' + MOD(level, 700), RPAD('x', 200, 'x')
     FROM dual CONNECT BY level <= 3000"
)
ora:::ora_execute(
  con,
  "CREATE TABLE ora_t_miot (
     id NUMBER, d DATE, v VARCHAR2(200),
     CONSTRAINT ora_t_miot_pk PRIMARY KEY (id, d))
   ORGANIZATION INDEX
   PARTITION BY RANGE (d) (
     PARTITION p1 VALUES LESS THAN (DATE '2024-01-01'),
     PARTITION p2 VALUES LESS THAN (MAXVALUE))"
)
ora:::ora_execute(
  con,
  "INSERT INTO ora_t_miot
   SELECT level, DATE '2023-06-01' + MOD(level, 400), RPAD('x', 200, 'x')
     FROM dual CONNECT BY level <= 3000"
)
ora:::ora_execute(con, "DELETE FROM ora_t_miot WHERE MOD(id, 10) <> 0")
ora:::ora_execute(con, "COMMIT")

# Helpers
expect_true(suppressMessages(ora:::ora_is_iot_top(con, me, "ORA_T_MIOT_PK")))
expect_false(suppressMessages(ora:::ora_is_iot_top(con, me, "ORA_T_HEAP_G")))
expect_equal(
  suppressMessages(ora:::ora_table_partitions(con, me, "ORA_T_HEAP")),
  c("P1", "P2", "P3")
)
info <- suppressMessages(ora:::ora_table_info(con, me, "ORA_T_MIOT"))
expect_true(info$iot && info$partitioned)
expect_equal(info$iot_index, "ORA_T_MIOT_PK")
expect_true(ora:::ora_segment_bytes(con, me, "ORA_T_HEAP") > 0)
expect_true(is.na(ora:::ora_segment_bytes(con, me, NA)))

# ora_sql_rebuild_index: IOT top index becomes MOVE PARTITION
r <- suppressMessages(ora::ora_sql_rebuild_index(con, me, "ora_t_miot_pk"))
expect_equal(nrow(r), 2L)
expect_true(all(grepl("^ALTER TABLE .* MOVE PARTITION", r$sql)))
expect_false(any(grepl("ONLINE", r$sql)))
r <- suppressMessages(
  ora::ora_sql_rebuild_index(con, me, "ORA_T_HEAP_L", partitions = "p2")
)
expect_equal(
  r$sql,
  sprintf("ALTER INDEX \"%s\".\"ORA_T_HEAP_L\" REBUILD PARTITION \"P2\"", me)
)
r <- suppressMessages(ora::ora_sql_rebuild_index(con, me, "ORA_T_HEAP_G"))
expect_equal(r$sql, sprintf("ALTER INDEX \"%s\".\"ORA_T_HEAP_G\" REBUILD", me))
expect_error(
  suppressMessages(ora::ora_sql_rebuild_index(con, me, "ORA_T_HEAP_G", "P1")),
  "not partitioned"
)

# ora_sql_shrink_partitions
sh <- suppressMessages(ora::ora_sql_shrink_partitions(con, me, "ORA_T_HEAP"))
expect_true(grepl("ENABLE ROW MOVEMENT$", sh$sql[1L]))
expect_equal(nrow(sh), 4L)
sh <- suppressMessages(
  ora::ora_sql_shrink_partitions(con, me, "ORA_T_MIOT", "p1", compact = TRUE)
)
expect_equal(nrow(sh), 1L)
expect_true(grepl("MODIFY PARTITION \"P1\" SHRINK SPACE COMPACT$", sh$sql))
expect_equal(sh$segment_name, "ORA_T_MIOT_PK")

# ora_sql_drop_partitions
expect_error(
  suppressMessages(
    ora::ora_sql_drop_partitions(con, me, "ORA_T_HEAP", c("P1", "P2", "P3"))
  ),
  "Refusing"
)
expect_error(
  suppressMessages(ora::ora_sql_drop_partitions(con, me, "ORA_T_HEAP", "P9")),
  "Unknown"
)
dp <- suppressMessages(ora::ora_sql_drop_partitions(
  con,
  me,
  "ORA_T_HEAP",
  "p1"
))
expect_true(grepl("DROP PARTITION \"P1\" UPDATE INDEXES$", dp$sql))

# ora_run_sql refuses without confirm
expect_error(
  capture.output(ora::ora_run_sql(con, dp)),
  "confirm = TRUE"
)
expect_equal(
  suppressMessages(ora:::ora_table_partitions(con, me, "ORA_T_HEAP")),
  c("P1", "P2", "P3")
)

# ora_run_sql runs shrink and drop
sh <- suppressMessages(
  ora::ora_sql_shrink_partitions(con, me, "ORA_T_MIOT", "P1")
)
log <- suppressMessages(
  ora::ora_run_sql(con, sh, confirm = TRUE, check_fra = fra_ok, unit = "B")
)
expect_equal(log$status, "ok")
expect_true(log$after_bytes <= log$before_bytes)
log <- suppressMessages(
  ora::ora_run_sql(con, dp, confirm = TRUE, check_fra = fra_ok)
)
expect_equal(log$status, "ok")
expect_equal(
  suppressMessages(ora:::ora_table_partitions(con, me, "ORA_T_HEAP")),
  c("P2", "P3")
)
st <- ora:::ora_query(
  con,
  "SELECT status FROM user_indexes WHERE index_name = 'ORA_T_HEAP_G'"
)
expect_equal(st$status, "VALID")

# ora_run_sql stops on the first error and returns the log
bad <- ora:::ora_sql_new(c(
  "ALTER TABLE ora_t_heap ENABLE ROW MOVEMENT",
  "ALTER TABLE no_such_table_xyz ENABLE ROW MOVEMENT",
  "ALTER TABLE ora_t_heap DISABLE ROW MOVEMENT"
))
err <- tryCatch(
  suppressMessages(ora::ora_run_sql(
    con,
    bad,
    confirm = TRUE,
    check_fra = FALSE
  )),
  ora_run_error = function(e) e
)
expect_inherits(err, "ora_run_error")
expect_equal(err$step, 2L)
expect_equal(nrow(err$log), 2L)
expect_equal(err$log$status[1L], "ok")
rm <- ora:::ora_query(
  con,
  "SELECT row_movement FROM user_tables WHERE table_name = 'ORA_T_HEAP'"
)
expect_equal(rm$row_movement, "ENABLED")

ora:::ora_execute(con, "DROP TABLE ora_t_heap PURGE")
ora:::ora_execute(con, "DROP TABLE ora_t_miot PURGE")
DBI::dbDisconnect(con)
