# ora_sql objects
s <- ora:::ora_sql_new(
  c(
    "ALTER TABLE \"A\".\"T\" MOVE PARTITION \"P1\"",
    "ALTER TABLE \"A\".\"T\" MOVE PARTITION \"P2\""
  ),
  "A",
  "T",
  "T_PK",
  c("P1", "P2")
)
expect_inherits(s, "ora_sql")
expect_inherits(s, "data.frame")
expect_equal(nrow(s), 2L)
expect_equal(s$partition_name, c("P1", "P2"))
expect_equal(as.character(s), s$sql)
expect_stdout(print(s), "MOVE PARTITION \"P2\"")
expect_stdout(print(s), "NOT executed")
expect_false(any(grepl("^--", as.character(s))))
expect_inherits(s[2, ], "ora_sql")
expect_error(ora:::ora_sql_new("DROP TABLE x;"), "semicolon")
expect_error(ora:::ora_sql_new(character(0)))

# ora_as_sql
x <- ora:::ora_as_sql(c("SELECT 1 FROM dual", "SELECT 2 FROM dual"))
expect_inherits(x, "ora_sql")
expect_true(all(is.na(x$segment_name)))
expect_identical(ora:::ora_as_sql(s), s)

# ora_sql_bind
b <- ora:::ora_sql_bind(ora:::ora_sql_new("SELECT 1 FROM dual"), s)
expect_inherits(b, "ora_sql")
expect_equal(nrow(b), 3L)
expect_equal(b$sql[1L], "SELECT 1 FROM dual")

# ora_match_partitions
existing <- c("P1", "P2", "P3")
expect_equal(ora:::ora_match_partitions(NULL, existing, "T"), existing)
expect_equal(
  ora:::ora_match_partitions(c("p2", "P1", "p2"), existing, "T"),
  c("P2", "P1")
)
expect_error(ora:::ora_match_partitions("P9", existing, "T"), "Unknown.*P9")
expect_error(ora:::ora_match_partitions("", existing, "T"))

# ora_no_partitions
expect_null(ora:::ora_no_partitions(NULL, "T"))
expect_error(ora:::ora_no_partitions("P1", "T"), "not partitioned")

# ora_run_error and ora_run_log
row <- data.frame(
  step = 1L,
  sql = "x",
  start_time = Sys.time(),
  end_time = Sys.time(),
  seconds = 0,
  before_bytes = 1024^3,
  after_bytes = 0,
  saved_bytes = 1024^3,
  status = "ok"
)
log <- ora:::ora_run_log(list(row, row), "GB")
expect_equal(nrow(log), 2L)
expect_true(all(c("before_gb", "after_gb", "saved_gb") %in% names(log)))
expect_equal(log$saved_gb, c(1, 1))
e <- ora:::ora_run_error("ORA-00054: resource busy", 2L, log)
expect_inherits(e, "ora_run_error")
expect_inherits(e, "ora_error")
expect_true(grepl("Step 2 failed", conditionMessage(e)))
expect_equal(nrow(e$log), 2L)
