# Initialization Guide

The directories contain initialization scripts for MySQL, OceanBase in MySQL mode,
Oracle, PostgreSQL, and openGauss.

## To Shenyu Users

- mysql/schema.sql

  > The canonical initialization script for both MySQL and OceanBase in MySQL mode.

  For OceanBase, execute this script against a MySQL-mode tenant with an account
  allowed to create the `shenyu` database. For example, using `obclient`:

  ```sh
  obclient -h <host> -P <port> -u '<user>@<tenant>' -p < db/init/mysql/schema.sql
  ```

  Start ShenYu Admin with the `ob` Spring profile (`--spring.profiles.active=ob`)
  and configure `application-ob.yml` for the tenant. Keep the `oceanbase` database
  dialect, `jdbc:oceanbase://` URL and `com.oceanbase.jdbc.Driver`.
  This script is for fresh installations and recreates tables. Existing OceanBase
  installations must use their version-specific `*-ob.sql` scripts in `db/upgrade`.
  OceanBase Oracle mode is outside the scope of this shared MySQL initialization.

- oracle/schema.sql

  > The file is the Oracle initialization script.

- pg/create-database.sql、pg/create-table.sql

  > The files are the PostgreSql initialization script.

- og/create-table.sql

  > The file is the openGauss initialization script.

## Maintaining the shared MySQL/OceanBase schema

Update `mysql/schema.sql` for both engines. The native SQL matrix validates MySQL
8.0 and OceanBase 4.3.5 LTS; it does not establish compatibility with every version.

The shared source retains the MySQL scaling tables and permissions, MCP selector,
namespace/lookup secondary indexes, and all common unique constraints. The old
OceanBase permission/object, resource/parent and user-role/user indexes are covered
by the shared indexes with the same leading columns. API-key permission IDs differ
between the old copies, but their role/resource grants are identical.

Both engines now use the current aiProxy fallback fields and aiPrompt handles;
the obsolete aiProxy `prompt` field is absent from `AiProxyHandle`. Token limiter
dictionaries use the supported `contextPath` resolver instead of the old unsupported
`default` entry. The loggingKafka plugin and default namespace use the same topic,
sampling/body-size/compression defaults and the `bootstrapServer` field consumed
by the Kafka plugin. Permissions pointing to absent resources have been removed
from fresh seeds; every existing resource retains its admin grant.

Historical upgrade scripts remain separate and unchanged. The Admin distribution
packages the whole `db` directory, including this guide and the shared script.

