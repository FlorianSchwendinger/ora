#' Assert a DBI connection
#'
#' @param con an object to check.
#' @return `con`, invisibly.
#' @noRd
assert_dbi_con <- function(con) {
  checkmate::assert_class(con, "DBIConnection", .var.name = "con")
  invisible(con)
}

#' Regex for one Oracle identifier, without anchors
#' @noRd
ora_ident_re <- "[A-Za-z][A-Za-z0-9_$#]*"

#' Pattern for a plain Oracle identifier
#' @noRd
ora_ident_pattern <- sprintf("^%s$", ora_ident_re)

#' Check name arguments and uppercase them
#'
#' @param x a character vector of names.
#' @param len the required length, or `NULL` for any length >= 1.
#' @param .var.name the name used in error messages.
#' @return the uppercased `x`.
#' @noRd
ora_as_name <- function(x, len = NULL, .var.name = checkmate::vname(x)) {
  checkmate::assert_character(
    x,
    min.chars = 1L,
    any.missing = FALSE,
    min.len = 1L,
    len = len,
    .var.name = .var.name
  )
  toupper(x)
}

#' Comma separated `?` placeholders
#'
#' @param n the number of placeholders.
#' @return a string such as `"?, ?, ?"`.
#' @noRd
ora_placeholders <- function(n) {
  checkmate::assert_count(n, positive = TRUE)
  paste(rep("?", n), collapse = ", ")
}

#' SQL condition that excludes Oracle-maintained schemas
#'
#' @param col the owner column to filter, an internal constant.
#' @return an SQL fragment.
#' @noRd
ora_owner_filter <- function(col = "owner") {
  checkmate::assert_string(col, pattern = "^[a-z_.]+$")
  sprintf(
    "%s IN (SELECT username FROM all_users WHERE oracle_maintained = 'N')",
    col
  )
}

#' Validate, uppercase and double-quote Oracle identifiers
#'
#' @param x a character vector of identifiers.
#' @return a character vector of quoted identifiers, e.g. `"SCOTT"`.
#' @noRd
ora_quote_ident <- function(x) {
  checkmate::assert_character(x, min.len = 1L, any.missing = FALSE)
  bad <- !grepl(ora_ident_pattern, x) | nchar(x) > 128L
  if (any(bad)) {
    stop(
      sprintf("Invalid Oracle identifier: %s", paste(x[bad], collapse = ", ")),
      call. = FALSE
    )
  }
  sprintf("\"%s\"", toupper(x))
}

#' Quote plain or owner-qualified names
#'
#' @param x a character vector such as `"DBA_USERS"` or `"SYS.DBA_USERS"`.
#' @return a character vector such as `"SYS"."DBA_USERS"`.
#' @noRd
ora_quote_name <- function(x) {
  checkmate::assert_character(x, min.len = 1L, any.missing = FALSE)
  pattern <- sprintf("^%s([.]%s)?$", ora_ident_re, ora_ident_re)
  bad <- !grepl(pattern, x)
  if (any(bad)) {
    stop(
      sprintf("Invalid Oracle name: %s", paste(x[bad], collapse = ", ")),
      call. = FALSE
    )
  }
  parts <- strsplit(x, ".", fixed = TRUE)
  quote_parts <- function(p) paste(ora_quote_ident(p), collapse = ".")
  vapply(parts, quote_parts, character(1L))
}

#' Size units and their power of 1024
#' @noRd
ora_units <- c(B = 0L, KB = 1L, MB = 2L, GB = 3L, TB = 4L)

#' Convert bytes to a unit
#'
#' @param bytes a numeric vector of bytes.
#' @param unit a size unit, one of `"B"`, `"KB"`, `"MB"`, `"GB"` or `"TB"`
#'   (powers of 1024).
#' @return a numeric vector, rounded to 2 decimals unless `unit = "B"`.
#' @noRd
ora_bytes_to_unit <- function(bytes, unit = "GB") {
  checkmate::assert_numeric(bytes)
  checkmate::assert_choice(unit, names(ora_units))
  x <- as.numeric(bytes)
  if (unit == "B") {
    return(x)
  }
  round(x / 1024^ora_units[[unit]], 2L)
}

#' Column name suffix for a unit
#'
#' @param unit a size unit, one of `names(ora_units)`.
#' @return `"bytes"` for `"B"`, else the lower case unit.
#' @noRd
ora_unit_suffix <- function(unit) {
  checkmate::assert_choice(unit, names(ora_units))
  if (unit == "B") "bytes" else tolower(unit)
}

#' Convert `*_bytes` columns to a unit and rename them
#'
#' @param df a data.frame.
#' @param cols the names of the `*_bytes` columns.
#' @param unit a size unit, one of `names(ora_units)`.
#' @return `df` with e.g. `used_bytes` replaced by `used_gb`.
#' @noRd
ora_apply_unit <- function(df, cols, unit = "GB") {
  checkmate::assert_data_frame(df)
  checkmate::assert_character(cols, pattern = "_bytes$", any.missing = FALSE)
  checkmate::assert_subset(cols, names(df))
  checkmate::assert_choice(unit, names(ora_units))
  for (col in cols) {
    df[[col]] <- ora_bytes_to_unit(df[[col]], unit)
  }
  new_names <- paste(sub("_bytes$", "", cols), ora_unit_suffix(unit), sep = "_")
  names(df)[match(cols, names(df))] <- new_names
  df
}

#' Lowercase the column names of a data.frame
#'
#' @param df a data.frame.
#' @return `df` with lower case names.
#' @noRd
ora_normalize_names <- function(df) {
  names(df) <- tolower(names(df))
  df
}

#' Convert Oracle Y/N and YES/NO flags to logical
#'
#' @param x a character vector of Y/N or YES/NO flags.
#' @return a logical vector, `NA` stays `NA`.
#' @noRd
ora_yes_no <- function(x) {
  checkmate::assert_character(x)
  res <- toupper(x) %in% c("Y", "YES")
  res[is.na(x)] <- NA
  res
}
