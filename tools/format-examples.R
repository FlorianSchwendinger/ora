#!/usr/bin/env Rscript
# Format roxygen @examples / @examplesIf blocks in R/*.R with air.
#
# Usage (from the package root):
#   Rscript tools/format-examples.R          # rewrite files
#   Rscript tools/format-examples.R --check  # exit 1 if a file would change

check <- "--check" %in% commandArgs(trailingOnly = TRUE)
prefix <- "#' "
width <- 80L - nchar(prefix)

# air reads air.toml from the directory of --stdin-file-path
cfg_dir <- tempfile("air-examples-")
dir.create(cfg_dir)
writeLines(
  c(
    "[format]",
    "indent-width = 2",
    "indent-style = \"space\"",
    sprintf("line-width = %d", width)
  ),
  file.path(cfg_dir, "air.toml")
)
stdin_path <- file.path(cfg_dir, "example.R")

air_format <- function(code) {
  out <- system2(
    "air",
    c("format", "--stdin-file-path", shQuote(stdin_path)),
    input = code,
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("air failed:\n", paste(out, collapse = "\n"), call. = FALSE)
  }
  while (length(out) > 0L && !nzchar(out[length(out)])) {
    out <- out[-length(out)]
  }
  out
}

strip_prefix <- function(lines) {
  sub("^#' ?", "", lines)
}

add_prefix <- function(lines) {
  ifelse(nzchar(lines), paste0(prefix, lines), "#'")
}

format_file <- function(path) {
  lines <- readLines(path)
  out <- character(0)
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    line <- lines[i]
    out <- c(out, line)
    i <- i + 1L
    if (!grepl("^#' @examples(If .*)?$", line)) {
      next
    }
    start <- i
    while (i <= n && grepl("^#'", lines[i]) && !grepl("^#' @", lines[i])) {
      i <- i + 1L
    }
    if (i > start) {
      code <- strip_prefix(lines[start:(i - 1L)])
      out <- c(out, add_prefix(air_format(code)))
    }
  }
  changed <- !identical(out, lines)
  if (changed && !check) {
    writeLines(out, path)
  }
  changed
}

files <- list.files("R", pattern = "[.]R$", full.names = TRUE)
changed <- vapply(files, format_file, logical(1L))
unlink(cfg_dir, recursive = TRUE)

for (f in files[changed]) {
  message(if (check) "would reformat: " else "reformatted: ", f)
}
if (check && any(changed)) {
  quit(status = 1L)
}
