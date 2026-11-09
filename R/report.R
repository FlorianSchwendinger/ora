#' Split table arguments into owner and table name
#'
#' @param tables a character vector of `"OWNER.TABLE"` or plain table names,
#'   or `NULL`.
#' @param owner the uppercased schema names, used for plain table names.
#' @return a data.frame with columns `owner` and `table_name`.
#' @noRd
ora_report_tables <- function(tables, owner) {
  if (is.null(tables)) {
    return(data.frame(owner = character(0), table_name = character(0)))
  }
  tables <- ora_as_name(tables)
  ora_quote_name(tables)
  qualified <- grepl(".", tables, fixed = TRUE)
  if (any(!qualified) && length(owner) != 1L) {
    stop(
      "Use OWNER.TABLE in `tables`, or give exactly one `owner`.",
      call. = FALSE
    )
  }
  parts <- strsplit(tables, ".", fixed = TRUE)
  first <- vapply(parts, `[`, character(1L), 1L)
  last <- vapply(parts, `[`, character(1L), 2L)
  data.frame(
    owner = ifelse(qualified, first, if (length(owner) == 1L) owner else NA),
    table_name = ifelse(qualified, last, first)
  )
}

#' Write an HTML space report
#'
#' Renders one self-contained HTML file with the sections switched on.
#' Read-only: cleanup SQL is only printed. Sections that need views the
#' user cannot read show a "Skipped" note instead of failing, and results
#' from `ALL_*` fallbacks are marked partial. Needs rmarkdown, knitr and
#' pandoc.
#'
#' @inheritParams ora_db_fullness
#' @param owner a character vector of schema names for the detail sections,
#'   or `NULL` for overview sections only.
#' @param tables a character vector of partitioned tables for the partition
#'   section, as `"OWNER.TABLE"` or plain names when `owner` has one schema.
#' @param file the path of the HTML file to write.
#' @param show_privileges a flag, show version, user and readable views.
#' @param show_fullness a flag, show database, tablespace, temp and FRA
#'   usage.
#' @param show_schemas a flag, show schema sizes.
#' @param show_segments a flag, show the biggest segments and per-owner
#'   tables, indexes, LOBs and recycle bin.
#' @param show_iot a flag, show IOTs per owner.
#' @param show_partitions a flag, show partition bounds and estimates per
#'   table.
#' @param show_activity a flag, show untouched tables and unused indexes.
#'   Needs `V$` views.
#' @param show_cleanup a flag, show generated rebuild and shrink SQL.
#' @param open a flag, open the report in a browser.
#' @return the path of the written file, invisibly.
#' @export
#' @examplesIf ora_has_example_db() && rmarkdown::pandoc_available()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' file <- tempfile(fileext = ".html")
#' ora_space_report(
#'   con,
#'   owner = Sys.getenv("ORA_USER"),
#'   file = file,
#'   open = FALSE
#' )
#' DBI::dbDisconnect(con)
ora_space_report <- function(
  con,
  owner = NULL,
  tables = NULL,
  file = "ora-space-report.html",
  unit = "GB",
  show_privileges = TRUE,
  show_fullness = TRUE,
  show_schemas = TRUE,
  show_segments = TRUE,
  show_iot = TRUE,
  show_partitions = TRUE,
  show_activity = FALSE,
  show_cleanup = FALSE,
  open = interactive()
) {
  assert_dbi_con(con)
  if (!is.null(owner)) {
    owner <- unique(ora_as_name(owner))
  }
  table_df <- ora_report_tables(tables, owner)
  checkmate::assert_path_for_output(file, overwrite = TRUE)
  checkmate::assert_choice(unit, names(ora_units))
  flags <- list(
    show_privileges = show_privileges,
    show_fullness = show_fullness,
    show_schemas = show_schemas,
    show_segments = show_segments,
    show_iot = show_iot,
    show_partitions = show_partitions,
    show_activity = show_activity,
    show_cleanup = show_cleanup
  )
  for (nm in names(flags)) {
    checkmate::assert_flag(flags[[nm]], .var.name = nm)
  }
  checkmate::assert_flag(open)
  for (pkg in c("rmarkdown", "knitr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package %s is needed for the report.", pkg), call. = FALSE)
    }
  }
  if (!rmarkdown::pandoc_available()) {
    stop("pandoc is needed to render the report.", call. = FALSE)
  }
  template <- system.file("report", "space-report.Rmd", package = "ora")
  work <- tempfile("ora-report-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  file.copy(template, work)
  out_dir <- normalizePath(dirname(file))
  params <- c(
    list(con = con, owner = owner, tables = table_df, unit = unit),
    flags
  )
  # Allow rendering from inside another R Markdown document.
  old_opts <- options(knitr.duplicate.label = "allow")
  on.exit(options(old_opts), add = TRUE)
  out <- rmarkdown::render(
    file.path(work, "space-report.Rmd"),
    output_file = basename(file),
    output_dir = out_dir,
    intermediates_dir = work,
    params = params,
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  if (open) {
    utils::browseURL(out)
  }
  invisible(out)
}
