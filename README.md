# ora

Helpers to explore how full an Oracle database is, where the space goes and
which privileges the connecting user has. Built for Oracle 19c and users
with limited privileges: queries fall back from `DBA_*` to `ALL_*` views and
report partial results, and missing privileges raise an `ora_no_privilege`
condition that names the view.

The package is read-only. Maintenance SQL (rebuild, shrink, drop
partitions) is only generated as text and runs solely through
`ora_run_sql(con, sql, confirm = TRUE)`.

## Installation

```bash
R CMD build ora
R CMD INSTALL ora_*.tar.gz
```

Needs DBI, checkmate and glue. Connections use odbc with the Oracle
Instant Client ODBC driver.

## Example

```r
library(ora)
con <- DBI::dbConnect(
  odbc::odbc(),
  dsn = Sys.getenv("ORA_DSN"),
  uid = Sys.getenv("ORA_USER"),
  pwd = Sys.getenv("ORA_PASSWORD")
)

ora_db_fullness(con)
ora_plot_tablespaces(ora_tablespace_usage(con))
ora_table_sizes(con, "SALES_OWNER", unit = "MB")

bounds <- ora_partition_bounds(con, "SALES_OWNER", "SALES")
old <- bounds$partition_name[bounds$high_date <= as.Date("2021-01-01")]
sql <- ora_sql_drop_partitions(con, "SALES_OWNER", "SALES", old)
sql # review first
# ora_run_sql(con, sql, confirm = TRUE)

DBI::dbDisconnect(con)
```

See `vignette("finding-space", package = "ora")` for the full walkthrough.

## Tests

Unit tests run offline. Database tests run only against the Oracle XE test
container, see `inst/test-setup/README.md`.
