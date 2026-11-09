source("helper_db.R", local = TRUE)
con <- ora_test_con()
if (is.null(con)) {
  exit_file("ORA_TEST_DSN not set")
}
if (!ora::ora_can_read(con, "DBA_SEGMENTS")) {
  expect_error(ora::ora_schema_sizes(con), class = "ora_no_privilege")
  DBI::dbDisconnect(con)
  exit_file("DBA_SEGMENTS not readable")
}
me <- toupper(Sys.getenv("ORA_TEST_USER"))

drop_quietly <- function(table) {
  try(
    ora:::ora_execute(con, sprintf("DROP TABLE %s PURGE", table)),
    silent = TRUE
  )
}
for (t in c("ORA_T_LOB", "ORA_T_IOT", "ORA_T_BIN")) {
  drop_quietly(t)
}

ora:::ora_execute(con, "CREATE TABLE ora_t_lob (id NUMBER PRIMARY KEY, c CLOB)")
ora:::ora_execute(con, "INSERT INTO ora_t_lob VALUES (1, 'x')")
ora:::ora_execute(
  con,
  "CREATE TABLE ora_t_iot (id NUMBER PRIMARY KEY) ORGANIZATION INDEX"
)
ora:::ora_execute(con, "INSERT INTO ora_t_iot VALUES (1)")
ora:::ora_execute(con, "CREATE TABLE ora_t_bin (id NUMBER)")
ora:::ora_execute(con, "INSERT INTO ora_t_bin VALUES (1)")
ora:::ora_execute(con, "DROP TABLE ora_t_bin")

# ora_schema_sizes
s <- ora::ora_schema_sizes(con)
expect_equal(names(s), c("owner", "segments", "size_gb", "pct"))
expect_true(me %in% s$owner)
expect_false("SYS" %in% s$owner)
expect_true("SYS" %in% ora::ora_schema_sizes(con, include_oracle = TRUE)$owner)

# ora_schema_breakdown
b <- ora::ora_schema_breakdown(con, me, unit = "B")
expect_true(all(c("TABLE", "INDEX", "LOBSEGMENT") %in% b$segment_type))
expect_equal(sum(b$pct), 100, tolerance = 0.1)

# ora_table_sizes
ts <- suppressMessages(ora::ora_table_sizes(con, me, unit = "B"))
lob <- ts[ts$table_name == "ORA_T_LOB", ]
expect_equal(nrow(lob), 1L)
expect_false(lob$iot)
expect_true(lob$data_bytes > 0 && lob$index_bytes > 0 && lob$lob_bytes > 0)
expect_equal(
  lob$total_bytes,
  lob$data_bytes + lob$index_bytes + lob$lob_bytes
)
iot <- ts[ts$table_name == "ORA_T_IOT", ]
expect_true(iot$iot)
expect_true(iot$data_bytes > 0)
expect_equal(iot$index_bytes, 0)

# ora_index_sizes
ix <- suppressMessages(ora::ora_index_sizes(con, me))
expect_true(any(ix$iot_top & ix$table_name == "ORA_T_IOT"))
expect_true(any(!ix$iot_top & ix$table_name == "ORA_T_LOB"))
expect_false("LOB" %in% ix$index_type)

# ora_lob_sizes
lb <- suppressMessages(ora::ora_lob_sizes(con, me, unit = "KB"))
row <- lb[lb$table_name == "ORA_T_LOB", ]
expect_equal(row$column_name, "C")
expect_true(row$size_kb > 0)

# ora_recyclebin_size
rb <- ora::ora_recyclebin_size(con, me)
expect_equal(rb$owner, me)
expect_true(rb$segments >= 1)

ora:::ora_execute(con, "PURGE RECYCLEBIN")
drop_quietly("ORA_T_LOB")
drop_quietly("ORA_T_IOT")
expect_equal(nrow(ora::ora_recyclebin_size(con, me)), 0L)

DBI::dbDisconnect(con)
