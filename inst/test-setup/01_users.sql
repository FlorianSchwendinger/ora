-- Run as SYSDBA. Statements end with "/" on its own line.
-- __PASSWORD__ is replaced by start-container.sh.
WHENEVER SQLERROR EXIT FAILURE
SET ECHO ON
ALTER SESSION SET CONTAINER = XEPDB1
/

-- Main test user and fixture owner: reads all DBA_* and V$ views.
GRANT SELECT_CATALOG_ROLE TO spaceapp
/
ALTER USER spaceapp QUOTA UNLIMITED ON users
/

-- Low-privilege user that mirrors production.
CREATE USER ora_lowpriv IDENTIFIED BY "__PASSWORD__"
/
GRANT CREATE SESSION TO ora_lowpriv
/
GRANT SELECT ON sys.dba_segments TO ora_lowpriv
/
GRANT SELECT ON sys.dba_data_files TO ora_lowpriv
/
GRANT SELECT ON sys.dba_tablespaces TO ora_lowpriv
/
GRANT SELECT ON sys.dba_tablespace_usage_metrics TO ora_lowpriv
/
GRANT SELECT ON sys.dba_sys_privs TO ora_lowpriv
/
GRANT SELECT ON sys.dba_role_privs TO ora_lowpriv
/
GRANT SELECT ON sys.v_$version TO ora_lowpriv
/
EXIT
