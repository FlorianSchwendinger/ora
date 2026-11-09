-- Run as spaceapp. Statements end with "/" on its own line.
-- Reproduces the production situation described in handoff.md.
SET ECHO ON

-- Drop leftovers from an earlier run, ignore errors.
WHENEVER SQLERROR CONTINUE
DROP TABLE sales PURGE
/
DROP TABLE measurements PURGE
/
DROP TABLE documents PURGE
/
DROP TABLE old_stuff PURGE
/
PURGE RECYCLEBIN
/
WHENEVER SQLERROR EXIT FAILURE

-- Monthly range-partitioned heap table with local and global indexes.
CREATE TABLE sales (
  sale_id     NUMBER NOT NULL,
  sale_date   DATE NOT NULL,
  customer_id NUMBER NOT NULL,
  amount      NUMBER(12, 2),
  note        VARCHAR2(200)
)
PARTITION BY RANGE (sale_date)
INTERVAL (NUMTOYMINTERVAL(1, 'MONTH'))
(PARTITION p_start VALUES LESS THAN (DATE '2020-01-01'))
/
CREATE INDEX sales_date_ix ON sales (sale_date) LOCAL
/
CREATE INDEX sales_customer_ix ON sales (customer_id)
/
INSERT INTO sales
SELECT level, DATE '2020-01-01' + MOD(level, 1826), MOD(level, 997),
       MOD(level, 1000) / 10, RPAD('sale', 100, 'x')
  FROM dual CONNECT BY level <= 60000
/
COMMIT
/

-- Partitioned IOT with many small monthly partitions.
CREATE TABLE measurements (
  sensor_id NUMBER NOT NULL,
  ts        DATE NOT NULL,
  val       NUMBER,
  payload   VARCHAR2(200),
  CONSTRAINT measurements_pk PRIMARY KEY (sensor_id, ts)
)
ORGANIZATION INDEX
PARTITION BY RANGE (ts) (
     PARTITION p2023_01 VALUES LESS THAN (DATE '2023-02-01'),
     PARTITION p2023_02 VALUES LESS THAN (DATE '2023-03-01'),
     PARTITION p2023_03 VALUES LESS THAN (DATE '2023-04-01'),
     PARTITION p2023_04 VALUES LESS THAN (DATE '2023-05-01'),
     PARTITION p2023_05 VALUES LESS THAN (DATE '2023-06-01'),
     PARTITION p2023_06 VALUES LESS THAN (DATE '2023-07-01'),
     PARTITION p2023_07 VALUES LESS THAN (DATE '2023-08-01'),
     PARTITION p2023_08 VALUES LESS THAN (DATE '2023-09-01'),
     PARTITION p2023_09 VALUES LESS THAN (DATE '2023-10-01'),
     PARTITION p2023_10 VALUES LESS THAN (DATE '2023-11-01'),
     PARTITION p2023_11 VALUES LESS THAN (DATE '2023-12-01'),
     PARTITION p2023_12 VALUES LESS THAN (DATE '2024-01-01'),
     PARTITION p2024_01 VALUES LESS THAN (DATE '2024-02-01'),
     PARTITION p2024_02 VALUES LESS THAN (DATE '2024-03-01'),
     PARTITION p2024_03 VALUES LESS THAN (DATE '2024-04-01'),
     PARTITION p2024_04 VALUES LESS THAN (DATE '2024-05-01'),
     PARTITION p2024_05 VALUES LESS THAN (DATE '2024-06-01'),
     PARTITION p2024_06 VALUES LESS THAN (DATE '2024-07-01'),
     PARTITION p2024_07 VALUES LESS THAN (DATE '2024-08-01'),
     PARTITION p2024_08 VALUES LESS THAN (DATE '2024-09-01'),
     PARTITION p2024_09 VALUES LESS THAN (DATE '2024-10-01'),
     PARTITION p2024_10 VALUES LESS THAN (DATE '2024-11-01'),
     PARTITION p2024_11 VALUES LESS THAN (DATE '2024-12-01'),
     PARTITION p2024_12 VALUES LESS THAN (DATE '2025-01-01'),
     PARTITION p2025_01 VALUES LESS THAN (DATE '2025-02-01'),
     PARTITION p2025_02 VALUES LESS THAN (DATE '2025-03-01'),
     PARTITION p2025_03 VALUES LESS THAN (DATE '2025-04-01'),
     PARTITION p2025_04 VALUES LESS THAN (DATE '2025-05-01'),
     PARTITION p2025_05 VALUES LESS THAN (DATE '2025-06-01'),
     PARTITION p2025_06 VALUES LESS THAN (DATE '2025-07-01'),
     PARTITION p2025_07 VALUES LESS THAN (DATE '2025-08-01'),
     PARTITION p2025_08 VALUES LESS THAN (DATE '2025-09-01'),
     PARTITION p2025_09 VALUES LESS THAN (DATE '2025-10-01'),
     PARTITION p2025_10 VALUES LESS THAN (DATE '2025-11-01'),
     PARTITION p2025_11 VALUES LESS THAN (DATE '2025-12-01'),
     PARTITION p2025_12 VALUES LESS THAN (DATE '2026-01-01'),
     PARTITION p_max VALUES LESS THAN (MAXVALUE)
)
/
INSERT INTO measurements
SELECT MOD(level, 50), DATE '2023-01-01' + level / 48, level,
       RPAD('m', 150, 'x')
  FROM dual CONNECT BY level <= 52000
/
COMMIT
/
-- Reclaimable space: delete most rows of the first six months.
DELETE FROM measurements
 WHERE ts < DATE '2023-07-01' AND MOD(sensor_id, 10) <> 0
/
COMMIT
/
-- Statistics after the delete, so shrink estimates show reclaimable space.
CALL dbms_stats.gather_table_stats(USER, 'MEASUREMENTS')
/

-- Table with CLOB and BLOB columns.
CREATE TABLE documents (
  doc_id NUMBER PRIMARY KEY,
  body   CLOB,
  image  BLOB
)
/
INSERT INTO documents
SELECT level, TO_CLOB(RPAD('text', 4000, 'x')),
       TO_BLOB(HEXTORAW(RPAD('AB', 4000, 'AB')))
  FROM dual CONNECT BY level <= 2000
/
COMMIT
/

-- A dropped table left in the recycle bin.
CREATE TABLE old_stuff AS
SELECT level AS id, RPAD('old', 200, 'x') AS txt
  FROM dual CONNECT BY level <= 5000
/
DROP TABLE old_stuff
/

-- Locked and stale statistics on sales.
CALL dbms_stats.gather_table_stats(USER, 'SALES')
/
INSERT INTO sales
SELECT 100000 + level, DATE '2024-06-01' + MOD(level, 180), MOD(level, 997),
       1, RPAD('late', 100, 'x')
  FROM dual CONNECT BY level <= 30000
/
COMMIT
/
CALL dbms_stats.lock_table_stats(USER, 'SALES')
/

-- Let the low-privilege user see the fixture in ALL_* views.
GRANT SELECT ON sales TO ora_lowpriv
/
GRANT SELECT ON measurements TO ora_lowpriv
/
GRANT SELECT ON documents TO ora_lowpriv
/
EXIT
