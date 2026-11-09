#' Run maintenance SQL
#'
#' The only function that changes the database. Refuses to run without
#' `confirm = TRUE`. Checks free space in the fast recovery area first,
#' then runs the statements one by one, logging time and segment size
#' before and after, and stops on the first error.
#'
#' The FRA check is rough: moves and shrinks are assumed to write about
#' the size of their segment as redo.
#'
#' @inheritParams ora_db_fullness
#' @param sql an `ora_sql` object from an `ora_sql_*()` function, or a
#'   character vector of statements.
#' @param confirm a flag, must be `TRUE` to run anything.
#' @param check_fra a flag, check free space in the fast recovery area.
#' @return a data.frame log, invisibly, with columns `step`, `sql`,
#'   `start_time`, `end_time`, `seconds`, `before_<unit>`, `after_<unit>`,
#'   `saved_<unit>` and `status`. On error an `ora_run_error` is raised
#'   with the log so far in `$log`.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' sql <- try(ora_sql_shrink_partitions(
#'   con,
#'   Sys.getenv("ORA_USER"),
#'   "MY_TABLE"
#' ))
#' try(ora_run_sql(con, sql))
#' DBI::dbDisconnect(con)
ora_run_sql <- function(
  con,
  sql,
  confirm = FALSE,
  check_fra = TRUE,
  unit = "GB"
) {
  assert_dbi_con(con)
  stmts <- ora_as_sql(sql)
  checkmate::assert_flag(confirm)
  checkmate::assert_flag(check_fra)
  checkmate::assert_choice(unit, names(ora_units))
  if (!confirm) {
    print(stmts)
    stop(
      "Nothing was run. Review the SQL above and call again with ",
      "confirm = TRUE.",
      call. = FALSE
    )
  }
  if (check_fra) {
    ora_check_fra(con, stmts)
  }
  n <- nrow(stmts)
  rows <- vector("list", n)
  for (i in seq_len(n)) {
    message(sprintf("[%d/%d] %s", i, n, stmts$sql[i]))
    rows[[i]] <- ora_run_step(con, stmts[i, ], i)
    if (rows[[i]]$status != "ok") {
      log <- ora_run_log(rows[seq_len(i)], unit)
      stop(ora_run_error(rows[[i]]$status, i, log))
    }
    message(sprintf("      done in %.1f s", rows[[i]]$seconds))
  }
  invisible(ora_run_log(rows, unit))
}

#' Run one statement and measure it
#'
#' @param con a DBI connection.
#' @param stmt a one-row `ora_sql` object.
#' @param step the step number.
#' @return a one-row data.frame log entry. `status` is `"ok"` or the error
#'   message.
#' @noRd
ora_run_step <- function(con, stmt, step) {
  before <- ora_try_segment_bytes(con, stmt)
  start <- Sys.time()
  status <- tryCatch(
    {
      ora_execute(con, stmt$sql, object = stmt$segment_name)
      "ok"
    },
    error = function(e) conditionMessage(e)
  )
  end <- Sys.time()
  after <- ora_try_segment_bytes(con, stmt)
  data.frame(
    step = step,
    sql = stmt$sql,
    start_time = start,
    end_time = end,
    seconds = round(as.numeric(difftime(end, start, units = "secs")), 2L),
    before_bytes = before,
    after_bytes = after,
    saved_bytes = before - after,
    status = status
  )
}

#' Segment size of a statement target, `NA` if unknown or not readable
#'
#' @param con a DBI connection.
#' @param stmt a one-row `ora_sql` object.
#' @return the size in bytes or `NA`.
#' @noRd
ora_try_segment_bytes <- function(con, stmt) {
  tryCatch(
    ora_segment_bytes(
      con,
      stmt$owner,
      stmt$segment_name,
      stmt$partition_name
    ),
    ora_no_privilege = function(e) NA_real_
  )
}

#' Combine log entries and convert sizes
#'
#' @param rows a list of one-row data.frames.
#' @param unit a size unit, one of `names(ora_units)`.
#' @return a data.frame.
#' @noRd
ora_run_log <- function(rows, unit) {
  log <- do.call(rbind, rows)
  ora_apply_unit(log, c("before_bytes", "after_bytes", "saved_bytes"), unit)
}

#' Build an `ora_run_error` condition
#'
#' @param msg the error message of the failed statement.
#' @param step the failed step number.
#' @param log the log so far.
#' @return a condition of class `ora_run_error`.
#' @noRd
ora_run_error <- function(msg, step, log) {
  structure(
    class = c("ora_run_error", "ora_error", "error", "condition"),
    list(
      message = sprintf("Step %d failed, later steps not run: %s", step, msg),
      call = NULL,
      step = step,
      log = log
    )
  )
}

#' Stop if the fast recovery area may be too small
#'
#' @param con a DBI connection.
#' @param stmts an `ora_sql` object.
#' @return `TRUE`, invisibly.
#' @noRd
ora_check_fra <- function(con, stmts) {
  fra <- tryCatch(
    ora_fra_usage(con, unit = "B"),
    ora_no_privilege = function(e) {
      stop(
        "Cannot read V$RECOVERY_FILE_DEST to check FRA space. ",
        "Use check_fra = FALSE to skip the check.",
        call. = FALSE
      )
    }
  )
  if (nrow(fra) == 0L) {
    message("No fast recovery area configured, FRA check skipped.")
    return(invisible(TRUE))
  }
  writes <- grepl("MOVE|SHRINK SPACE|REBUILD", stmts$sql)
  needed <- 0
  for (i in which(writes)) {
    bytes <- ora_try_segment_bytes(con, stmts[i, ])
    if (!is.na(bytes)) {
      needed <- needed + bytes
    }
  }
  if (needed > fra$free_bytes) {
    stop(
      sprintf(
        "FRA has %.2f GB free, the statements may write about %.2f GB.",
        ora_bytes_to_unit(fra$free_bytes),
        ora_bytes_to_unit(needed)
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
