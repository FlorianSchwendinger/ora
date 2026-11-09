# Error code extraction
msg <- "nanodbc.cpp:1783: [Oracle][ODBC][Ora]ORA-00942: table or view does not exist"
expect_equal(ora:::ora_error_code(msg), "ORA-00942")
expect_true(is.na(ora:::ora_error_code("some other error")))

# Privilege errors are classed
e <- simpleError("ORA-00942: table or view does not exist")
cond <- ora:::ora_as_condition(e, "DBA_USERS", "ALL_USERS")
expect_inherits(cond, "ora_no_privilege")
expect_inherits(cond, "ora_error")
expect_equal(cond$object, "DBA_USERS")
expect_equal(cond$code, "ORA-00942")
expect_equal(cond$fallback, "ALL_USERS")
expect_true(grepl("ALL_USERS", conditionMessage(cond)))
expect_error(stop(cond), class = "ora_no_privilege")

e <- simpleError("ORA-01418: specified index does not exist")
cond <- ora:::ora_as_condition(e, "SCOTT.IDX1")
expect_equal(cond$code, "ORA-01418")
expect_true(is.na(cond$fallback))
expect_true(grepl("ALTER ANY INDEX", conditionMessage(cond)))

expect_equal(
  ora:::ora_as_condition(
    simpleError("ORA-01031: insufficient privileges"),
    "V$SESSION"
  )$code,
  "ORA-01031"
)

# Other errors pass through unchanged
e <- simpleError("ORA-00904: invalid identifier")
expect_identical(ora:::ora_as_condition(e, "X"), e)

# Partial results message
m <- ora:::ora_partial("DBA_USERS", "ALL_USERS")
expect_inherits(m, "ora_partial")
expect_inherits(m, "message")
expect_message(message(m), "partial")
expect_true(grepl("DBA_USERS", conditionMessage(m)))

# Dictionary templates
tpl <- "SELECT index_name FROM {dict}_indexes WHERE owner = ?"
expect_equal(
  ora:::ora_render_dict(tpl, "dba"),
  "SELECT index_name FROM dba_indexes WHERE owner = ?"
)
expect_equal(
  ora:::ora_render_dict(tpl, "all"),
  "SELECT index_name FROM all_indexes WHERE owner = ?"
)
expect_false(inherits(ora:::ora_render_dict(tpl, "all"), "glue"))
expect_error(ora:::ora_render_dict(tpl, "user"))
