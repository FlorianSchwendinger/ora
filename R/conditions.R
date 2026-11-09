#' Privilege error codes and the privilege most likely missing
#' @noRd
ora_privilege_codes <- c(
  "ORA-00942" = "SELECT on the object (or the object does not exist)",
  "ORA-01031" = "a system or object privilege required by the statement",
  "ORA-01418" = "ALTER ANY INDEX (or ownership of the index)"
)

#' Extract the first ORA error code from a message
#'
#' @param msg an error message.
#' @return an ORA code such as `"ORA-00942"`, or `NA`.
#' @noRd
ora_error_code <- function(msg) {
  checkmate::assert_string(msg)
  m <- regmatches(msg, regexpr("ORA-[0-9]{5}", msg))
  if (length(m) == 0L) NA_character_ else m
}

#' Build an `ora_no_privilege` condition
#'
#' @param object the view or object that failed.
#' @param code an ORA error code from `ora_privilege_codes`.
#' @param fallback the name of a fallback view, or `NA`.
#' @param call the call to report.
#' @return a condition of class `ora_no_privilege`.
#' @noRd
ora_no_privilege <- function(
  object,
  code,
  fallback = NA_character_,
  call = NULL
) {
  checkmate::assert_string(object, na.ok = TRUE)
  checkmate::assert_choice(code, names(ora_privilege_codes))
  checkmate::assert_string(fallback, na.ok = TRUE)
  privilege <- ora_privilege_codes[[code]]
  msg <- sprintf("%s on %s. Likely missing: %s.", code, object, privilege)
  if (!is.na(fallback)) {
    msg <- paste(msg, sprintf("Fallback: %s.", fallback))
  }
  structure(
    class = c("ora_no_privilege", "ora_error", "error", "condition"),
    list(
      message = msg,
      call = call,
      object = object,
      code = code,
      privilege = privilege,
      fallback = fallback
    )
  )
}

#' Turn a driver error into `ora_no_privilege` if it is a privilege error
#'
#' @param e the condition raised by the driver.
#' @param object the view or object that failed.
#' @param fallback the name of a fallback view, or `NA`.
#' @return an `ora_no_privilege` condition, or `e` unchanged.
#' @noRd
ora_as_condition <- function(e, object, fallback = NA_character_) {
  code <- ora_error_code(conditionMessage(e))
  if (is.na(code) || !code %in% names(ora_privilege_codes)) {
    return(e)
  }
  ora_no_privilege(object, code, fallback, call = conditionCall(e))
}

#' Build an `ora_partial` message
#'
#' @param object the view that could not be read.
#' @param fallback the view used instead.
#' @return a condition of class `ora_partial`.
#' @noRd
ora_partial <- function(object, fallback) {
  checkmate::assert_string(object)
  checkmate::assert_string(fallback)
  msg <- sprintf(
    "No access to %s, using %s. Results are partial.\n",
    object,
    fallback
  )
  structure(
    class = c("ora_partial", "message", "condition"),
    list(message = msg, call = NULL, object = object, fallback = fallback)
  )
}
