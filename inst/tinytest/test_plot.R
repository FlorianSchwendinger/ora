grDevices::pdf(NULL)

ts <- data.frame(
  tablespace_name = c("USERS", "SYSAUX"),
  used_gb = c(410, 2.1),
  max_gb = c(480, 32),
  used_pct = c(85.42, 6.56)
)
expect_identical(ora::ora_plot_tablespaces(ts), ts)
expect_error(ora::ora_plot_tablespaces(ts[, c("tablespace_name", "used_pct")]))
expect_error(ora::ora_plot_tablespaces(ts[0, ]))

sc <- data.frame(owner = c("A", "B"), size_mb = c(10, 2))
expect_identical(ora::ora_plot_schemas(sc), sc)
expect_silent(ora::ora_plot_schemas(sc, n = 1))
expect_error(ora::ora_plot_schemas(data.frame(owner = "A", size = 1)), "size_")

tb <- data.frame(
  table_name = c("T1", "T2"),
  data_gb = c(1, 2),
  index_gb = c(0.5, 0),
  lob_gb = c(0, 3)
)
expect_identical(ora::ora_plot_table_sizes(tb), tb)
tb_mixed <- tb
names(tb_mixed)[names(tb_mixed) == "lob_gb"] <- "lob_mb"
expect_error(ora::ora_plot_table_sizes(tb_mixed), "one unit")

pb <- data.frame(
  partition_name = c("P1", "P2", "P3"),
  partition_position = c(1, 2, 3),
  high_date = as.Date(c("2024-01-01", "2024-02-01", NA)),
  size_bytes = c(100, 200, NA)
)
expect_identical(ora::ora_plot_partitions(pb), pb)
ps <- data.frame(partition_name = c("P2", "P1"), size_gb = c(0, 0))
expect_identical(ora::ora_plot_partitions(ps), ps)
many <- data.frame(
  partition_name = sprintf("P%03d", 1:300),
  size_mb = runif(300)
)
expect_identical(ora::ora_plot_partitions(many), many)

# Size column detection
expect_equal(ora:::ora_size_col(sc, "size"), list(col = "size_mb", unit = "MB"))
expect_equal(ora:::ora_size_col(pb, "size")$unit, "bytes")
expect_equal(ora:::ora_fmt_size(1234.5, "MB"), "1,234.50 MB")
expect_equal(ora:::ora_fmt_size(1234, "bytes"), "1,234 bytes")

grDevices::dev.off()
