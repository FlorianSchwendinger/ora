source("helper_db.R", local = TRUE)
con <- ora_test_con()
if (is.null(con)) {
  exit_file("ORA_TEST_DSN not set")
}

needed <- c("DBA_TABLESPACE_USAGE_METRICS", "DBA_TABLESPACES", "DBA_DATA_FILES")
if (all(ora::ora_can_read(con, needed))) {
  # ora_db_fullness
  f <- ora::ora_db_fullness(con)
  expect_equal(nrow(f), 1L)
  expect_equal(
    names(f),
    c("used_gb", "allocated_gb", "max_gb", "free_gb", "used_pct")
  )
  expect_true(f$used_gb <= f$max_gb)
  expect_true(f$used_pct >= 0 && f$used_pct <= 100)
  fb <- ora::ora_db_fullness(con, unit = "B")
  expect_equal(fb$max_bytes - fb$used_bytes, fb$free_bytes)

  # ora_tablespace_usage
  ts <- ora::ora_tablespace_usage(con, unit = "MB")
  expect_true(all(c("SYSTEM", "SYSAUX") %in% ts$tablespace_name))
  expect_false("TEMPORARY" %in% ts$contents)
  expect_true(all(
    c("used_mb", "allocated_mb", "max_mb", "free_mb") %in% names(ts)
  ))
  expect_false(is.unsorted(rev(ts$used_pct)))

  # ora_temp_usage
  tmp <- ora::ora_temp_usage(con)
  expect_true(nrow(tmp) >= 1L)
  expect_equal(
    names(tmp),
    c("tablespace_name", "status", "used_gb", "max_gb", "free_gb", "used_pct")
  )
} else {
  expect_error(ora::ora_db_fullness(con), class = "ora_no_privilege")
}

# ora_fra_usage needs V$RECOVERY_FILE_DEST
if (ora::ora_can_read(con, "V$RECOVERY_FILE_DEST")) {
  fra <- ora::ora_fra_usage(con)
  expect_true(nrow(fra) <= 1L)
  expect_true("free_gb" %in% names(fra))
} else {
  expect_error(ora::ora_fra_usage(con), class = "ora_no_privilege")
}

expect_error(ora::ora_db_fullness(con, unit = "GiB"))

DBI::dbDisconnect(con)
