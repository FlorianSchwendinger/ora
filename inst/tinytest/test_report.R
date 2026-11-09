# Table arguments
empty <- ora:::ora_report_tables(NULL, NULL)
expect_equal(nrow(empty), 0L)
expect_equal(names(empty), c("owner", "table_name"))

t1 <- ora:::ora_report_tables(c("hr.emp", "sales"), "APP")
expect_equal(t1$owner, c("HR", "APP"))
expect_equal(t1$table_name, c("EMP", "SALES"))

t2 <- ora:::ora_report_tables("a.b", c("X", "Y"))
expect_equal(t2$owner, "A")
expect_error(ora:::ora_report_tables("sales", c("X", "Y")), "OWNER.TABLE")
expect_error(ora:::ora_report_tables("sales", NULL), "OWNER.TABLE")
expect_error(ora:::ora_report_tables("a.b.c", NULL), "Invalid Oracle name")
expect_error(ora:::ora_report_tables("x; drop", "A"), "Invalid Oracle name")

# Template is installed
expect_true(nzchar(system.file("report", "space-report.Rmd", package = "ora")))
