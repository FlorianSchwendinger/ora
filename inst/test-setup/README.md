# Test database for ora

Integration tests run only against this container, never against
production.

## 1. Start the container

```bash
inst/test-setup/start-container.sh
```

Starts `gvenzl/oracle-xe:21-slim-faststart` with Podman (or Docker), waits
until it is ready and runs:

- `01_users.sql` as SYSDBA: `spaceapp` gets `SELECT_CATALOG_ROLE`, and
  `ora_lowpriv` gets only the grants the production user has (no
  `DBA_USERS`, no `V$` views except `V$VERSION`).
- `02_fixture.sql` as `spaceapp`: monthly heap partitions with local and
  global indexes, a partitioned IOT with many small partitions and deleted
  rows, a CLOB/BLOB table, a dropped table in the recycle bin and locked,
  stale statistics.

XE 21c is newer than production 19c. The package uses 19c features only.

## 2. Configure ODBC

Install unixODBC and the Oracle Instant Client basic and odbc packages,
then add the entries from `odbcinst.ini.example` and `odbc.ini.example`.

Without root, unpack Instant Client into e.g. `~/opt/oracle`, put the full
driver path in `~/.odbc.ini` and set `LD_LIBRARY_PATH`, because the driver
does not find `libclntsh` on its own:

```ini
[ora-test]
Driver = /home/<user>/opt/oracle/instantclient_23_26/libsqora.so.23.1
ServerName = //localhost:1521/XEPDB1
```

```bash
export LD_LIBRARY_PATH=$HOME/opt/oracle/instantclient_23_26
```

## 3. Run the tests

```bash
export ORA_TEST_DSN=ora-test
export ORA_TEST_USER=spaceapp
export ORA_TEST_PASSWORD=Test_pw_123
export ORA_TEST_LOWPRIV_USER=ora_lowpriv
export ORA_TEST_LOWPRIV_PASSWORD=Test_pw_123
R CMD INSTALL ora
Rscript -e 'tinytest::test_package("ora")'
```

Without `ORA_TEST_DSN` the database tests exit early.

## 4. Remove the container

```bash
podman rm -f ora-test
```
