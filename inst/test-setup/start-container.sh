#!/usr/bin/env bash
# Start an Oracle XE test container and load users and fixtures.
# Never point the tests at production.
#
# Usage: inst/test-setup/start-container.sh
# Env:   ORA_CONTAINER_ENGINE (podman or docker, default: whichever exists)
#        ORA_CONTAINER_NAME   (default: ora-test)
#        ORA_TEST_PASSWORD    (default: Test_pw_123, test container only)
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
name="${ORA_CONTAINER_NAME:-ora-test}"
password="${ORA_TEST_PASSWORD:-Test_pw_123}"
image="docker.io/gvenzl/oracle-xe:21-slim-faststart"
engine="${ORA_CONTAINER_ENGINE:-}"

if [ -z "$engine" ]
then
  if command -v podman >/dev/null 2>&1
  then
    engine="podman"
  elif command -v docker >/dev/null 2>&1
  then
    engine="docker"
  else
    echo "Neither podman nor docker found." >&2
    exit 1
  fi
fi

if "$engine" container inspect "$name" >/dev/null 2>&1
then
  echo "Container $name exists. Remove it with: $engine rm -f $name" >&2
  exit 1
fi

echo "Starting $image as $name with $engine"
"$engine" run -d --name "$name" -p 1521:1521 \
  -e ORACLE_PASSWORD="$password" \
  -e APP_USER=spaceapp \
  -e APP_USER_PASSWORD="$password" \
  "$image" >/dev/null

echo "Waiting for the database (up to 10 minutes)"
for _ in $(seq 1 120)
do
  if "$engine" logs "$name" 2>&1 | grep "DATABASE IS READY TO USE" >/dev/null
  then
    break
  fi
  sleep 5
done
if ! "$engine" logs "$name" 2>&1 | grep "DATABASE IS READY TO USE" >/dev/null
then
  echo "Database not ready, see: $engine logs $name" >&2
  exit 1
fi

echo "Creating users"
sed "s/__PASSWORD__/$password/g" "$here/01_users.sql" |
  "$engine" exec -i "$name" sqlplus -s -l / as sysdba

echo "Creating fixtures"
"$engine" exec -i "$name" \
  sqlplus -s -l "spaceapp/$password@//localhost:1521/XEPDB1" \
  <"$here/02_fixture.sql"

cat <<MSG
Done. Connect to service XEPDB1 on localhost:1521.
  main user:    spaceapp    / $password
  low-priv user: ora_lowpriv / $password
Remove with: $engine rm -f $name
MSG
