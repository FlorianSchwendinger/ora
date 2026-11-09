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

try(ora:::ora_execute(con, "DROP TABLE ora_t_iv PURGE"), silent = TRUE)
ora:::ora_execute(
  con,
  "CREATE TABLE ora_t_iv (id NUMBER, d DATE, v VARCHAR2(200))
   PARTITION BY RANGE (d) INTERVAL (NUMTOYMINTERVAL(1, 'MONTH'))
   (PARTITION p0 VALUES LESS THAN (DATE '2024-01-01'))"
)
ora:::ora_execute(con, "CREATE INDEX ora_t_iv_l ON ora_t_iv (d) LOCAL")
ora:::ora_execute(con, "CREATE INDEX ora_t_iv_g ON ora_t_iv (id)")
ora:::ora_execute(
  con,
  "INSERT INTO ora_t_iv
   SELECT level, DATE '2023-12-15' + MOD(level, 90), RPAD('x', 200, 'x')
     FROM dual CONNECT BY level <= 3000"
)
ora:::ora_execute(con, "COMMIT")

# ora_partition_bounds
b <- suppressMessages(ora::ora_partition_bounds(con, me, "ora_t_iv", "B"))
expect_equal(b$partition_name[1L], "P0")
expect_equal(b$high_date[1L], as.Date("2024-01-01"))
expect_true(is.na(b$low_date[1L]))
expect_equal(b$low_date[2L], b$high_date[1L])
expect_false(b$interval[1L])
expect_true(all(b$interval[-1L]))
expect_equal(b$high_date[2L], as.Date("2024-02-01"))
expect_true(all(b$size_bytes > 0))
expect_true(is.character(b$high_value))
expect_equal(
  b$partition_position,
  seq_len(nrow(b))
)

# ora_top_segments
top <- ora::ora_top_segments(con, n = 5, unit = "B")
expect_equal(nrow(top), 5L)
expect_false(is.unsorted(rev(top$size_bytes)))
expect_false(any(grepl("PARTITION", top$segment_type)))
expect_false("SYS" %in% top$owner)
top_me <- ora::ora_top_segments(con, me, n = 3, unit = "B")
expect_true(nrow(top_me) >= 1L && nrow(top_me) <= 3L)
expect_equal(unique(top_me$owner), me)
expect_true("ORA_T_IV" %in% top_me$segment_name)
expect_true("SYS" %in% ora::ora_top_segments(con, "sys", n = 1)$owner)
expect_error(ora::ora_top_segments(con, c("A", "B")))

# ora_partition_estimates
ora:::ora_execute(con, "CALL dbms_stats.gather_table_stats(USER, 'ORA_T_IV')")
pe <- suppressMessages(ora::ora_partition_estimates(con, me, "ORA_T_IV", "B"))
expect_equal(pe$partition_name, b$partition_name)
expect_equal(sum(pe$num_rows), 3000)
expect_equal(pe$waste_bytes, pe$segment_bytes - pe$estimated_bytes)
expect_true(all(pe$reliable))
expect_true(all(c("initial_bytes", "reclaimable_bytes") %in% names(pe)))
expect_true(all(pe$reclaimable_bytes >= 0))
expect_true(all(pe$reclaimable_bytes <= pmax(pe$waste_bytes, 0)))
# small interval partitions sit at their first extent: nothing to shrink
expect_true(all(
  pe$reclaimable_bytes[pe$segment_bytes == pe$initial_bytes] == 0
))

# ora_unusable_indexes: none, then a moved partition and a dropped one
expect_false(
  "ORA_T_IV_G" %in%
    suppressMessages(ora::ora_unusable_indexes(con, me))$index_name
)
ora:::ora_execute(con, "ALTER TABLE ora_t_iv MOVE PARTITION p0")
ora:::ora_execute(
  con,
  sprintf("ALTER TABLE ora_t_iv DROP PARTITION %s", b$partition_name[2L])
)
ui <- suppressMessages(ora::ora_unusable_indexes(con, me))
expect_true(any(ui$index_name == "ORA_T_IV_G" & ui$object_level == "INDEX"))
expect_true(any(
  ui$index_name == "ORA_T_IV_L" &
    ui$object_level == "PARTITION" &
    ui$partition_name == "P0"
))
expect_true(all(
  ui$table_name[ui$index_name %in% c("ORA_T_IV_G", "ORA_T_IV_L")] == "ORA_T_IV"
))

# ora_table_locks: a second session holds an uncommitted insert
if (ora::ora_can_read(con, "V$LOCKED_OBJECT")) {
  con2 <- ora_test_con()
  DBI::dbBegin(con2)
  DBI::dbExecute(
    con2,
    "INSERT INTO ora_t_iv VALUES (-1, DATE '2024-03-01', 'x')"
  )
  lk <- suppressMessages(ora::ora_table_locks(con, me, "ora_t_iv"))
  expect_true(nrow(lk) >= 1L)
  expect_true("ROW EXCLUSIVE" %in% lk$locked_mode)
  expect_equal(unique(lk$username), me)
  DBI::dbRollback(con2)
  DBI::dbDisconnect(con2)
  lk <- suppressMessages(ora::ora_table_locks(con, me, "ORA_T_IV"))
  expect_equal(nrow(lk), 0L)
} else {
  expect_error(ora::ora_table_locks(con, me), class = "ora_no_privilege")
}

ora:::ora_execute(con, "DROP TABLE ora_t_iv PURGE")
DBI::dbDisconnect(con)
