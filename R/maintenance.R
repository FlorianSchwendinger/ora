#' Return SQL to rebuild an index
#'
#' Returns SQL code for you to inspect and run, nothing is executed. Run it
#' with [ora_run_sql()]. For an `IOT - TOP` index, `ALTER INDEX ... REBUILD`
#' fails (ORA-28650), so `ALTER TABLE ... MOVE [PARTITION]` is generated
#' instead, without `ONLINE`. Partitioned indexes are rebuilt per partition.
#'
#' @inheritParams ora_schema_breakdown
#' @param index the index name.
#' @param partitions a character vector of partition names, or `NULL` for
#'   all partitions.
#' @return an `ora_sql` object with the SQL statements, not executed.
#'   Inspect it, then run it yourself with
#'   `ora_run_sql(con, sql, confirm = TRUE)`.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' try(ora_sql_rebuild_index(con, Sys.getenv("ORA_USER"), "MY_INDEX"))
#' DBI::dbDisconnect(con)
ora_sql_rebuild_index <- function(con, owner, index, partitions = NULL) {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  index <- ora_as_name(index, len = 1L)
  info <- ora_index_info(con, owner, index)
  q_owner <- ora_quote_ident(owner)
  if (info$index_type == "IOT - TOP") {
    tab <- paste(q_owner, ora_quote_ident(info$table_name), sep = ".")
    if (!info$partitioned) {
      ora_no_partitions(partitions, index)
      return(ora_sql_new(
        glue::glue("ALTER TABLE {tab} MOVE"),
        owner,
        info$table_name,
        index
      ))
    }
    existing <- ora_table_partitions(con, owner, info$table_name)
    parts <- ora_match_partitions(partitions, existing, info$table_name)
    q_parts <- ora_quote_ident(parts)
    sql <- glue::glue("ALTER TABLE {tab} MOVE PARTITION {q_parts}")
    return(ora_sql_new(sql, owner, info$table_name, index, parts))
  }
  idx <- paste(q_owner, ora_quote_ident(index), sep = ".")
  if (!info$partitioned) {
    ora_no_partitions(partitions, index)
    return(ora_sql_new(
      glue::glue("ALTER INDEX {idx} REBUILD"),
      owner,
      info$table_name,
      index
    ))
  }
  existing <- ora_index_partitions(con, owner, index)
  parts <- ora_match_partitions(partitions, existing, index)
  q_parts <- ora_quote_ident(parts)
  sql <- glue::glue("ALTER INDEX {idx} REBUILD PARTITION {q_parts}")
  ora_sql_new(sql, owner, info$table_name, index, parts)
}

#' Return SQL to shrink table partitions
#'
#' Returns SQL code for you to inspect and run, nothing is executed. Run it
#' with [ora_run_sql()]. Uses `MODIFY PARTITION ... SHRINK SPACE`, also for
#' IOTs. For heap tables without row movement, `ENABLE ROW MOVEMENT` is added
#' first. Needs an ASSM tablespace.
#'
#' @inheritParams ora_estimate_data_size
#' @param partitions a character vector of partition names, or `NULL` for
#'   all partitions.
#' @param compact a flag, add `COMPACT` (defragment only, keep the high
#'   water mark).
#' @return an `ora_sql` object with the SQL statements, not executed.
#'   Inspect it, then run it yourself with
#'   `ora_run_sql(con, sql, confirm = TRUE)`.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' try(ora_sql_shrink_partitions(con, Sys.getenv("ORA_USER"), "MY_TABLE"))
#' DBI::dbDisconnect(con)
ora_sql_shrink_partitions <- function(
  con,
  owner,
  table,
  partitions = NULL,
  compact = FALSE
) {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  table <- ora_as_name(table, len = 1L)
  checkmate::assert_flag(compact)
  info <- ora_table_info(con, owner, table)
  tab <- ora_quote_name(paste(owner, table, sep = "."))
  segment <- if (info$iot) info$iot_index else table
  suffix <- if (compact) " COMPACT" else ""
  if (info$partitioned) {
    existing <- ora_table_partitions(con, owner, table)
    parts <- ora_match_partitions(partitions, existing, table)
    q_parts <- ora_quote_ident(parts)
    sql <- glue::glue(
      "ALTER TABLE {tab} MODIFY PARTITION {q_parts} SHRINK SPACE{suffix}"
    )
  } else {
    ora_no_partitions(partitions, table)
    parts <- NA_character_
    sql <- glue::glue("ALTER TABLE {tab} SHRINK SPACE{suffix}")
  }
  res <- ora_sql_new(sql, owner, table, segment, parts)
  if (!info$iot && !info$row_movement) {
    pre <- ora_sql_new(
      glue::glue("ALTER TABLE {tab} ENABLE ROW MOVEMENT"),
      owner,
      table
    )
    res <- ora_sql_bind(pre, res)
  }
  res
}

#' Return SQL to drop table partitions
#'
#' Returns SQL code for you to inspect and run, nothing is executed. Run it
#' with [ora_run_sql()]. Never chooses partitions itself and refuses to drop
#' all of them. Uses `UPDATE INDEXES` so global indexes stay usable.
#'
#' @inheritParams ora_estimate_data_size
#' @param partitions a character vector of partition names to drop.
#' @return an `ora_sql` object with the SQL statements, not executed.
#'   Inspect it, then run it yourself with
#'   `ora_run_sql(con, sql, confirm = TRUE)`.
#' @export
#' @examplesIf ora_has_example_db()
#' con <- DBI::dbConnect(
#'   odbc::odbc(),
#'   dsn = Sys.getenv("ORA_DSN"),
#'   uid = Sys.getenv("ORA_USER"),
#'   pwd = Sys.getenv("ORA_PASSWORD")
#' )
#' try(ora_sql_drop_partitions(
#'   con,
#'   Sys.getenv("ORA_USER"),
#'   "MY_TABLE",
#'   c("P2019_01", "P2019_02")
#' ))
#' DBI::dbDisconnect(con)
ora_sql_drop_partitions <- function(con, owner, table, partitions) {
  assert_dbi_con(con)
  owner <- ora_as_name(owner, len = 1L)
  table <- ora_as_name(table, len = 1L)
  partitions <- ora_as_name(partitions)
  info <- ora_table_info(con, owner, table)
  if (!info$partitioned) {
    stop(
      sprintf("Table %s.%s is not partitioned.", owner, table),
      call. = FALSE
    )
  }
  existing <- ora_table_partitions(con, owner, table)
  parts <- ora_match_partitions(partitions, existing, table)
  if (length(setdiff(existing, parts)) == 0L) {
    stop(
      sprintf("Refusing to drop all partitions of %s.%s.", owner, table),
      call. = FALSE
    )
  }
  tab <- ora_quote_name(paste(owner, table, sep = "."))
  q_parts <- ora_quote_ident(parts)
  sql <- glue::glue("ALTER TABLE {tab} DROP PARTITION {q_parts} UPDATE INDEXES")
  segment <- if (info$iot) info$iot_index else table
  ora_sql_new(sql, owner, table, segment, parts)
}

#' Stop if partitions are given for a non-partitioned object
#'
#' @param partitions the `partitions` argument.
#' @param object the table or index name.
#' @return `NULL`, invisibly.
#' @noRd
ora_no_partitions <- function(partitions, object) {
  if (!is.null(partitions)) {
    stop(sprintf("%s is not partitioned.", object), call. = FALSE)
  }
  invisible(NULL)
}
