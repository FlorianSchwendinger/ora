#' Colors and neutrals for plots
#'
#' Categorical slots blue, orange, aqua (validated for color vision
#' deficiency), a light blue track and recessive grays for text, grid and
#' axes.
#' @noRd
ora_viz <- list(
  series = c("#2a78d6", "#eb6834", "#1baf7a"),
  track = "#cde2fb",
  text = "#0b0b0b",
  secondary = "#52514e",
  muted = "#898781",
  grid = "#e1e0d9",
  axis = "#c3c2b7",
  surface = "#ffffff"
)

#' Find the size column with a given prefix
#'
#' @param x a data.frame from an `ora_*()` function.
#' @param prefix the column prefix, e.g. `"size"` for `size_gb`.
#' @return a list with `col` (column name) and `unit` (axis label).
#' @noRd
ora_size_col <- function(x, prefix) {
  pattern <- sprintf("^%s_(bytes|kb|mb|gb|tb)$", prefix)
  col <- grep(pattern, names(x), value = TRUE)
  if (length(col) != 1L) {
    stop(
      sprintf("x needs exactly one %s_<unit> column.", prefix),
      call. = FALSE
    )
  }
  unit <- sub(pattern, "\\1", col)
  list(col = col, unit = if (unit == "bytes") "bytes" else toupper(unit))
}

#' Format sizes for value labels
#'
#' @param x a numeric vector.
#' @param unit the unit label.
#' @return a character vector.
#' @noRd
ora_fmt_size <- function(x, unit) {
  digits <- if (unit == "bytes") 0L else 2L
  paste(formatC(x, format = "f", digits = digits, big.mark = ","), unit)
}

#' Draw horizontal bars, stacked by column, largest at the top
#'
#' @param mat a numeric matrix, one row per bar, one column per segment.
#' @param labels the bar labels.
#' @param value_text the text at the bar ends.
#' @param main the title.
#' @param xlab the axis title.
#' @param track the length of a background track per bar, or `NULL`.
#' @param legend_names the segment names for a legend, or `NULL`.
#' @return `NULL`, invisibly.
#' @noRd
ora_hbar <- function(
  mat,
  labels,
  value_text,
  main,
  xlab,
  track = NULL,
  legend_names = NULL
) {
  n <- nrow(mat)
  ends <- rowSums(mat)
  xmax <- max(c(ends, track, 0), na.rm = TRUE)
  if (xmax <= 0) {
    xmax <- 1
  }
  cin <- graphics::par("cin")[1L]
  left <- max(nchar(labels)) * cin * 0.8 + 0.3
  right <- max(nchar(value_text)) * cin * 0.7 + 0.3
  top <- if (is.null(legend_names)) 0.6 else 0.9
  op <- graphics::par(
    mai = c(0.9, left, top, right),
    bg = ora_viz$surface,
    xpd = FALSE
  )
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(0, xmax),
    ylim = c(0.5, n + 0.5),
    xaxs = "i",
    yaxs = "i"
  )
  ticks <- pretty(c(0, xmax))
  ticks <- ticks[ticks <= xmax]
  graphics::abline(v = ticks, col = ora_viz$grid, lwd = 1)
  y <- rev(seq_len(n))
  # half bar thickness: at most 0.1 inch, so bars stay thin
  h <- min(0.35, 0.1 * n / graphics::par("pin")[2L])
  if (!is.null(track)) {
    graphics::rect(0, y - h, track, y + h, col = ora_viz$track, border = NA)
  }
  start <- rep(0, n)
  for (j in seq_len(ncol(mat))) {
    graphics::rect(
      start,
      y - h,
      start + mat[, j],
      y + h,
      col = ora_viz$series[j],
      border = ora_viz$surface,
      lwd = 1.5
    )
    start <- start + mat[, j]
  }
  graphics::segments(0, 0.5, 0, n + 0.5, col = ora_viz$axis, lwd = 1)
  graphics::axis(
    1,
    at = ticks,
    lwd = 0,
    lwd.ticks = 1,
    col.ticks = ora_viz$axis,
    col.axis = ora_viz$muted,
    cex.axis = 0.8
  )
  graphics::par(xpd = TRUE)
  graphics::text(
    0,
    y,
    labels,
    pos = 2,
    col = ora_viz$text,
    cex = 0.85
  )
  bar_end <- pmax(ends, if (is.null(track)) 0 else track)
  graphics::text(
    bar_end,
    y,
    value_text,
    pos = 4,
    col = ora_viz$secondary,
    cex = 0.8
  )
  graphics::title(main = main, adj = 0, col.main = ora_viz$text, line = top * 3)
  graphics::title(xlab = xlab, col.lab = ora_viz$secondary, cex.lab = 0.85)
  if (!is.null(legend_names)) {
    graphics::legend(
      "bottomleft",
      inset = c(0, 1),
      legend = legend_names,
      fill = ora_viz$series[seq_along(legend_names)],
      border = NA,
      bty = "n",
      horiz = TRUE,
      text.col = ora_viz$text,
      cex = 0.8
    )
  }
  invisible(NULL)
}

#' Plot tablespace usage
#'
#' Used space as bars over a light track up to the maximum size, fullest
#' first, with the used percentage at the bar end.
#'
#' @param x a data.frame from [ora_tablespace_usage()] or
#'   [ora_temp_usage()].
#' @param n the number of tablespaces to show.
#' @param main the title.
#' @return `x`, invisibly.
#' @export
#' @examples
#' x <- data.frame(
#'   tablespace_name = c("USERS", "SYSAUX", "SYSTEM", "UNDOTBS1"),
#'   used_gb = c(410, 2.1, 1.2, 0.3),
#'   max_gb = c(480, 32, 32, 32),
#'   used_pct = c(85.42, 6.56, 3.75, 0.94)
#' )
#' ora_plot_tablespaces(x)
ora_plot_tablespaces <- function(x, n = 20L, main = "Tablespace usage") {
  checkmate::assert_data_frame(x, min.rows = 1L)
  checkmate::assert_names(
    names(x),
    must.include = c("tablespace_name", "used_pct")
  )
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_string(main)
  used <- ora_size_col(x, "used")
  max_col <- ora_size_col(x, "max")
  d <- x[order(-x$used_pct), ]
  d <- d[seq_len(min(n, nrow(d))), ]
  ora_hbar(
    mat = matrix(d[[used$col]], ncol = 1L),
    labels = d$tablespace_name,
    value_text = sprintf("%.0f%%", d$used_pct),
    main = main,
    xlab = sprintf("Used of max size (%s)", used$unit),
    track = d[[max_col$col]]
  )
  invisible(x)
}

#' Plot the largest schemas
#'
#' @param x a data.frame from [ora_schema_sizes()].
#' @param n the number of schemas to show.
#' @param main the title.
#' @return `x`, invisibly.
#' @export
#' @examples
#' x <- data.frame(
#'   owner = c("SALES", "HR", "STAGE"),
#'   segments = c(1200, 40, 300),
#'   size_gb = c(512.4, 3.2, 48.9),
#'   pct = c(90.8, 0.6, 8.6)
#' )
#' ora_plot_schemas(x)
ora_plot_schemas <- function(x, n = 15L, main = "Largest schemas") {
  checkmate::assert_data_frame(x, min.rows = 1L)
  checkmate::assert_names(names(x), must.include = "owner")
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_string(main)
  size <- ora_size_col(x, "size")
  d <- x[order(-x[[size$col]]), ]
  d <- d[seq_len(min(n, nrow(d))), ]
  ora_hbar(
    mat = matrix(d[[size$col]], ncol = 1L),
    labels = d$owner,
    value_text = ora_fmt_size(d[[size$col]], size$unit),
    main = main,
    xlab = sprintf("Size (%s)", size$unit)
  )
  invisible(x)
}

#' Plot the largest tables split into data, indexes and LOBs
#'
#' @param x a data.frame from [ora_table_sizes()].
#' @param n the number of tables to show.
#' @param main the title.
#' @return `x`, invisibly.
#' @export
#' @examples
#' x <- data.frame(
#'   table_name = c("MEASUREMENTS", "SALES", "DOCUMENTS"),
#'   data_gb = c(120, 80, 2),
#'   index_gb = c(0, 25, 0.1),
#'   lob_gb = c(0, 0, 40),
#'   total_gb = c(120, 105, 42.1)
#' )
#' ora_plot_table_sizes(x)
ora_plot_table_sizes <- function(x, n = 15L, main = "Largest tables") {
  checkmate::assert_data_frame(x, min.rows = 1L)
  checkmate::assert_names(names(x), must.include = "table_name")
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_string(main)
  cols <- list(
    ora_size_col(x, "data"),
    ora_size_col(x, "index"),
    ora_size_col(x, "lob")
  )
  units <- unique(vapply(cols, `[[`, character(1L), "unit"))
  if (length(units) != 1L) {
    stop("data, index and lob columns must share one unit.", call. = FALSE)
  }
  col_names <- vapply(cols, `[[`, character(1L), "col")
  mat <- as.matrix(x[, col_names])
  ord <- order(-rowSums(mat))
  keep <- ord[seq_len(min(n, length(ord)))]
  mat <- mat[keep, , drop = FALSE]
  ora_hbar(
    mat = mat,
    labels = x$table_name[keep],
    value_text = ora_fmt_size(rowSums(mat), units),
    main = main,
    xlab = sprintf("Size (%s)", units),
    legend_names = c("Data", "Indexes", "LOBs")
  )
  invisible(x)
}

#' Plot size per partition in partition order
#'
#' Shows whether old partitions are large or small at a glance. With
#' [ora_partition_bounds()] the axis shows the upper date bound, with
#' [ora_partition_sizes()] partitions are ordered by name.
#'
#' @param x a data.frame from [ora_partition_bounds()] or
#'   [ora_partition_sizes()].
#' @param main the title.
#' @return `x`, invisibly.
#' @export
#' @examples
#' x <- data.frame(
#'   partition_name = sprintf("P2024_%02d", 1:12),
#'   partition_position = 1:12,
#'   high_date = seq(as.Date("2024-02-01"), by = "month", length.out = 12),
#'   size_gb = c(
#'     0.55,
#'     0.52,
#'     0.5,
#'     0.48,
#'     0.5,
#'     0.31,
#'     0.3,
#'     0.29,
#'     0.3,
#'     0.28,
#'     0.27,
#'     0.12
#'   )
#' )
#' ora_plot_partitions(x)
ora_plot_partitions <- function(x, main = "Size per partition") {
  checkmate::assert_data_frame(x, min.rows = 1L)
  checkmate::assert_names(names(x), must.include = "partition_name")
  checkmate::assert_string(main)
  size <- ora_size_col(x, "size")
  if ("partition_position" %in% names(x)) {
    d <- x[order(x$partition_position), ]
  } else {
    d <- x[order(x$partition_name), ]
  }
  vals <- d[[size$col]]
  vals[is.na(vals)] <- 0
  n <- length(vals)
  has_dates <- "high_date" %in% names(d) && any(!is.na(d$high_date))
  if (has_dates) {
    labels <- ifelse(
      is.na(d$high_date),
      "MAX",
      format(as.Date(d$high_date), "%Y-%m-%d")
    )
    xlab <- "Partition (upper date bound)"
  } else {
    labels <- d$partition_name
    xlab <- "Partition"
  }
  ymax <- max(vals, 0)
  if (ymax <= 0) {
    ymax <- 1
  }
  op <- graphics::par(mai = c(1.4, 0.9, 0.6, 0.3), bg = ora_viz$surface)
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(0.5, n + 0.5),
    ylim = c(0, ymax * 1.1),
    xaxs = "i",
    yaxs = "i"
  )
  ticks <- pretty(c(0, ymax))
  ticks <- ticks[ticks <= ymax * 1.1]
  graphics::abline(h = ticks, col = ora_viz$grid, lwd = 1)
  w <- if (n > 150L) 0.5 else 0.4
  border <- if (n > 150L) NA else ora_viz$surface
  graphics::rect(
    seq_len(n) - w,
    0,
    seq_len(n) + w,
    vals,
    col = ora_viz$series[1L],
    border = border,
    lwd = 1.5
  )
  graphics::segments(0.5, 0, n + 0.5, 0, col = ora_viz$axis, lwd = 1)
  graphics::axis(
    2,
    at = ticks,
    las = 1,
    lwd = 0,
    lwd.ticks = 1,
    col.ticks = ora_viz$axis,
    col.axis = ora_viz$muted,
    cex.axis = 0.8
  )
  at <- unique(round(seq(1, n, length.out = min(n, 8L))))
  graphics::axis(
    1,
    at = at,
    labels = labels[at],
    las = 2,
    lwd = 0,
    lwd.ticks = 1,
    col.ticks = ora_viz$axis,
    col.axis = ora_viz$muted,
    cex.axis = 0.75
  )
  big <- which.max(vals)
  graphics::text(
    big,
    vals[big],
    ora_fmt_size(vals[big], size$unit),
    pos = 3,
    col = ora_viz$secondary,
    cex = 0.8,
    xpd = TRUE
  )
  graphics::title(main = main, adj = 0, col.main = ora_viz$text)
  graphics::title(
    ylab = sprintf("Size (%s)", size$unit),
    col.lab = ora_viz$secondary,
    cex.lab = 0.85
  )
  graphics::mtext(
    xlab,
    side = 1,
    line = 5,
    col = ora_viz$secondary,
    cex = 0.85
  )
  invisible(x)
}
