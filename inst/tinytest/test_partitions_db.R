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

try(ora:::ora_execute(con, "DROP TABLE ora_t_piot PURGE"), silent = TRUE)
ora:::ora_execute(
  con,
  "CREATE TABLE ora_t_piot (
     id NUMBER, d DATE, v VARCHAR2(100),
     CONSTRAINT ora_t_piot_pk PRIMARY KEY (id, d))
   ORGANIZATION INDEX
   PARTITION BY RANGE (d) (
     PARTITION p2024 VALUES LESS THAN (DATE '2025-01-01'),
     PARTITION p2025 VALUES LESS THAN (DATE '2026-01-01'),
     PARTITION pmax VALUES LESS THAN (MAXVALUE))"
)
ora:::ora_execute(
  con,
  "INSERT INTO ora_t_piot
   SELECT level, DATE '2024-06-01' + MOD(level, 500), RPAD('x', 50, 'x')
     FROM dual CONNECT BY level <= 1000"
)
ora:::ora_execute(con, "COMMIT")

# ora_iot_tables
iot <- suppressMessages(ora::ora_iot_tables(con, me, unit = "B"))
row <- iot[iot$table_name == "ORA_T_PIOT", ]
expect_equal(nrow(row), 1L)
expect_equal(row$index_name, "ORA_T_PIOT_PK")
expect_true(row$partitioned)
expect_true(row$segments >= 2)
expect_true(row$max_segment_bytes <= row$size_bytes)

# ora_partition_sizes, by table name of an IOT
ps <- suppressMessages(ora::ora_partition_sizes(con, me, "ora_t_piot"))
expect_true(all(c("P2024", "P2025") %in% ps$partition_name))
expect_true(all(ps$segment_type == "INDEX PARTITION"))
expect_equal(
  nrow(suppressMessages(
    ora::ora_partition_sizes(con, me, "ORA_T_PIOT", n = 1)
  )),
  1L
)

# ora_partition_summary
sm <- suppressMessages(ora::ora_partition_summary(con, me, "ORA_T_PIOT", "B"))
expect_equal(sm$partitions, nrow(ps))
expect_equal(sm$min_partition, min(ps$partition_name))
ps_b <- suppressMessages(
  ora::ora_partition_sizes(con, me, "ORA_T_PIOT", unit = "B")
)
expect_equal(sm$total_bytes, sum(ps_b$size_bytes))

# ora_count_rows
cnt <- suppressMessages(ora::ora_count_rows(con, me, "ORA_T_PIOT"))
expect_equal(cnt$num_rows, 1000)
expect_message(ora::ora_count_rows(con, me, "ORA_T_PIOT"), "slow")
p24 <- suppressMessages(ora::ora_count_rows(con, me, "ORA_T_PIOT", "p2024"))
ref <- ora:::ora_query(
  con,
  "SELECT COUNT(*) AS n FROM ora_t_piot WHERE d < DATE '2025-01-01'"
)
expect_equal(p24$num_rows, as.numeric(ref$n))
expect_equal(p24$partition_name, "P2024")
expect_error(
  suppressMessages(ora::ora_count_rows(con, me, "NO_SUCH_TABLE_XYZ")),
  class = "ora_no_privilege"
)

# ora_estimate_data_size
ora:::ora_execute(con, "CALL dbms_stats.gather_table_stats(USER, 'ORA_T_PIOT')")
est <- suppressMessages(ora::ora_estimate_data_size(con, me, "ORA_T_PIOT"))
expect_equal(est$num_rows, 1000)
expect_true(est$reliable)
expect_false(est$stats_locked)
expect_true(est$segment_gb >= 0)
ora:::ora_execute(con, "CALL dbms_stats.lock_table_stats(USER, 'ORA_T_PIOT')")
est <- suppressMessages(ora::ora_estimate_data_size(con, me, "ORA_T_PIOT"))
expect_true(est$stats_locked)
expect_false(est$reliable)
expect_error(
  suppressMessages(ora::ora_estimate_data_size(con, me, "NO_SUCH_TABLE_XYZ")),
  "not found"
)

ora:::ora_execute(con, "DROP TABLE ora_t_piot PURGE")
DBI::dbDisconnect(con)
