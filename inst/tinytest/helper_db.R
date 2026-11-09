# Open a connection to the Oracle test container, or NULL if not configured.
# user = "main" uses ORA_TEST_USER, "lowpriv" uses ORA_TEST_LOWPRIV_USER.
ora_test_con <- function(user = "main") {
  prefix <- switch(
    user,
    main = "ORA_TEST_",
    lowpriv = "ORA_TEST_LOWPRIV_",
    stop("Unknown test user: ", user)
  )
  dsn <- Sys.getenv("ORA_TEST_DSN")
  uid <- Sys.getenv(paste0(prefix, "USER"))
  if (!nzchar(dsn) || !nzchar(uid)) {
    return(NULL)
  }
  if (!requireNamespace("odbc", quietly = TRUE)) {
    return(NULL)
  }
  DBI::dbConnect(
    odbc::odbc(),
    dsn = dsn,
    uid = uid,
    pwd = Sys.getenv(paste0(prefix, "PASSWORD"))
  )
}
