# HIGH_VALUE parsing
hv <- c(
  "TO_DATE(' 2024-01-01 00:00:00', 'SYYYY-MM-DD HH24:MI:SS', 'NLS_CALENDAR=GREGORIAN')",
  "TIMESTAMP' 2025-06-01 00:00:00'",
  "MAXVALUE",
  "100",
  NA
)
expect_equal(
  ora:::ora_parse_high_value(hv),
  as.Date(c("2024-01-01", "2025-06-01", NA, NA, NA))
)
expect_equal(ora:::ora_parse_high_value(character(0)), as.Date(character(0)))
expect_error(ora:::ora_parse_high_value(1))

# Lock modes
expect_equal(
  unname(ora:::ora_lock_modes[c("3", "6")]),
  c("ROW EXCLUSIVE", "EXCLUSIVE")
)

# Reclaimable space
expect_equal(
  ora:::ora_reclaimable(
    segment = c(8, 8, 20, 20, 8),
    initial = c(8, 1, 1, 1, 8),
    estimated = c(0.1, 2, 5, 25, NA)
  ),
  c(0, 6, 15, 0, NA)
)
