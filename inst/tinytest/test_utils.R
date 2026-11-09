# Identifier quoting
expect_equal(ora:::ora_quote_ident("scott"), "\"SCOTT\"")
expect_equal(
  ora:::ora_quote_ident(c("t1", "A$B#C_1")),
  c("\"T1\"", "\"A$B#C_1\"")
)
expect_error(ora:::ora_quote_ident("1abc"), "Invalid Oracle identifier")
expect_error(ora:::ora_quote_ident("a b"), "Invalid Oracle identifier")
expect_error(ora:::ora_quote_ident("x\" OR 1=1"), "Invalid Oracle identifier")
expect_error(
  ora:::ora_quote_ident(strrep("a", 129L)),
  "Invalid Oracle identifier"
)
expect_error(ora:::ora_quote_ident(NA_character_))

# Size conversion
expect_equal(ora:::ora_bytes_to_unit(1024^3 * 1.234), 1.23)
expect_equal(ora:::ora_bytes_to_unit(c(0, 1024^3)), c(0, 1))
expect_equal(ora:::ora_bytes_to_unit(1024^2 * 5, "MB"), 5)
expect_equal(ora:::ora_bytes_to_unit(1024, "KB"), 1)
expect_equal(ora:::ora_bytes_to_unit(1024^4 / 2, "TB"), 0.5)
expect_error(ora:::ora_bytes_to_unit(1, "GiB"))
expect_error(ora:::ora_bytes_to_unit("1"))
expect_equal(ora:::ora_bytes_to_unit(1234567, "B"), 1234567)
expect_equal(ora:::ora_unit_suffix("B"), "bytes")
expect_equal(ora:::ora_unit_suffix("GB"), "gb")

# Column names
df <- data.frame(OWNER = "A", BYTES = 1)
expect_equal(names(ora:::ora_normalize_names(df)), c("owner", "bytes"))

# SQL check
expect_silent(ora:::ora_check_sql("SELECT 1 FROM dual"))
expect_error(ora:::ora_check_sql("SELECT 1 FROM dual;"), "semicolon")
expect_error(ora:::ora_check_sql("SELECT 1 FROM dual ;\n"), "semicolon")

# Connection assertion
expect_error(ora:::assert_dbi_con("not a connection"), "DBIConnection")

# Name arguments
expect_equal(ora:::ora_as_name(c("scott", "Emp")), c("SCOTT", "EMP"))
expect_error(ora:::ora_as_name(""))
expect_error(ora:::ora_as_name(NA_character_))
expect_error(ora:::ora_as_name(character(0)))
expect_error(ora:::ora_as_name(c("a", "b"), len = 1L))
owner <- 1
expect_error(ora:::ora_as_name(owner), "owner")

# Qualified names
expect_equal(ora:::ora_quote_name("dba_users"), "\"DBA_USERS\"")
expect_equal(
  ora:::ora_quote_name(c("sys.dba_users", "V$SESSION")),
  c("\"SYS\".\"DBA_USERS\"", "\"V$SESSION\"")
)
expect_error(ora:::ora_quote_name("a."), "Invalid Oracle name")
expect_error(ora:::ora_quote_name(".a"), "Invalid Oracle name")
expect_error(ora:::ora_quote_name("a.b.c"), "Invalid Oracle name")
expect_error(ora:::ora_quote_name("dual where 1=1"), "Invalid Oracle name")

# Placeholders
expect_equal(ora:::ora_placeholders(1L), "?")
expect_equal(ora:::ora_placeholders(3L), "?, ?, ?")
expect_error(ora:::ora_placeholders(0L))

# Owner filter
expect_true(grepl(
  "^s[.]owner IN [(]SELECT username FROM all_users",
  ora:::ora_owner_filter("s.owner")
))
expect_error(ora:::ora_owner_filter("owner OR 1=1"))

# Unit columns
df <- data.frame(owner = "A", used_bytes = 1024^3 * 2, max_bytes = 1024^3 * 4)
res <- ora:::ora_apply_unit(df, c("used_bytes", "max_bytes"))
expect_equal(names(res), c("owner", "used_gb", "max_gb"))
expect_equal(res$used_gb, 2)
res <- ora:::ora_apply_unit(df, "used_bytes", "MB")
expect_equal(names(res), c("owner", "used_mb", "max_bytes"))
expect_equal(res$used_mb, 2048)
res <- ora:::ora_apply_unit(df, "used_bytes", "B")
expect_equal(names(res), names(df))
expect_error(ora:::ora_apply_unit(df, "owner"))
expect_error(ora:::ora_apply_unit(df, "free_bytes"))

# Unit columns on an empty result
empty <- data.frame(name = character(0), used_bytes = numeric(0))
res <- ora:::ora_apply_unit(empty, "used_bytes")
expect_equal(names(res), c("name", "used_gb"))
expect_equal(nrow(res), 0L)

# Y/N flags
expect_equal(
  ora:::ora_yes_no(c("Y", "N", "YES", "NO", "yes", NA)),
  c(TRUE, FALSE, TRUE, FALSE, TRUE, NA)
)
expect_equal(ora:::ora_yes_no(character(0)), logical(0))
expect_error(ora:::ora_yes_no(1))
