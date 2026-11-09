# Grant tree SQL
anchor <- "SELECT privilege, grantee, CAST(NULL AS VARCHAR2(4000)), 1 FROM dba_sys_privs WHERE privilege IN (?)"
sql <- ora:::ora_grant_tree_sql(anchor)
expect_true(is.character(sql) && length(sql) == 1L)
expect_false(inherits(sql, "glue"))
expect_true(grepl(anchor, sql, fixed = TRUE))
expect_true(grepl("oracle_maintained = 'N'", sql, fixed = TRUE))
expect_false(grepl(";", sql, fixed = TRUE))
sql <- ora:::ora_grant_tree_sql(anchor, include_oracle = TRUE)
expect_true(grepl("WHERE 1 = 1", sql, fixed = TRUE))
expect_false(grepl("oracle_maintained", sql, fixed = TRUE))
expect_error(ora:::ora_grant_tree_sql(anchor, include_oracle = NA))
