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

- pg/create-database.sql, pg/create-table.sql

  > The canonical initialization scripts for PostgreSQL and openGauss.

  Create the `shenyu` database using `pg/create-database.sql` if it does not
  already exist, then execute `pg/create-table.sql` against that database.
  For example, for openGauss:

  ```sh
  gsql -h <host> -p <port> -U <user> -d postgres -v ON_ERROR_STOP=1 -f db/init/pg/create-database.sql
  gsql -h <host> -p <port> -U <user> -d shenyu -v ON_ERROR_STOP=1 -f db/init/pg/create-table.sql
  ```

  PostgreSQL users can run the same files with `psql` instead of `gsql`.
  Enter the database password when prompted; see the
  [gsql command reference](https://docs.opengauss.org/en/docs/latest-lite/tool_and_commandreference/gsql.html).
  Start ShenYu Admin with the `og` Spring profile (`--spring.profiles.active=og`)
  for openGauss and configure `application-og.yml`. Keep the `opengauss` database
  dialect, `jdbc:opengauss://` URL, `org.opengauss.Driver`, and openGauss MyBatis
  type handlers. PostgreSQL continues to use its `pg` profile and driver.

  The table script recreates tables and is only for fresh installations. Existing
  openGauss installations must use their version-specific `*-og.sql` scripts
  in `db/upgrade`; PostgreSQL installations use `*-pg.sql`.

## Fresh-install scope

### MySQL and OceanBase

This consolidation changes fresh **MySQL installations as well as OceanBase**,
not only the OceanBase script path. In the canonical `mysql/schema.sql`, orphan
permission seeds are removed, loggingKafka uses `bootstrapServer` instead of
`namesrvAddr`, and its default-namespace configuration is aligned with the plugin
configuration. Existing resource grants are retained. The upstream removal of
SOFA/TARS resources and their associated seeds is preserved.

These are initialization defaults, not an upgrade migration. Do not re-run the
initialization script against an existing database; continue to use the separate
version-specific MySQL or OceanBase scripts in `db/upgrade`.

### PostgreSQL and openGauss

This consolidation changes fresh **PostgreSQL installations as well as openGauss**,
not only the openGauss script path. The canonical `pg/create-table.sql` removes
orphan permissions, gives paramMapping and modifyResponse their custom rule-page
defaults, adds the request rule-page handle, and puts Kafka security handles at
selector scope. It also consolidates Dubbo's load-balancing handle, fixes
loggingKafka's `bootstrapServer` configuration, and aligns plugin/default-namespace
configs, including aiProxy fallback defaults without the obsolete `prompt` field.

PostgreSQL's table definitions and constraints are retained. Existing resource
grants remain available, and the upstream removal of SOFA/TARS resources and their
associated seeds is preserved. These are fresh-install defaults, not changes to
existing databases; historical PostgreSQL and openGauss upgrade scripts are unchanged.

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

Known cross-dialect difference: the H2 seed in
`shenyu-admin/src/main/resources/sql-script/h2/schema.sql` still uses
`DEFAULT_KEY_RESOLVER` (`default`) rather than `CONTEXT_PATH_KEY_RESOLVER`
(`contextPath`). `AiTokenLimiterEnum.getByName` falls back to `CONTEXT_PATH`
for the unsupported value. H2 dictionary alignment remains a separate follow-up;
this consolidation does not change the H2 schema.

Historical upgrade scripts remain separate and unchanged. The Admin distribution
packages the whole `db` directory, including this guide and the shared script.

## Maintaining the shared PostgreSQL/openGauss schema

Update `pg/create-table.sql` for both engines. The native SQL matrix verifies
PostgreSQL 15 and openGauss lite 5.0.1 using this complete shared source, without
a compatibility overlay. Storage Compose and Kubernetes E2E setup copy the same
file. The Admin distribution packages the whole `db` directory, so the shared
script and this guide remain available in release packages.

The former copies differed in the following ways:

- The discovery-upstream index now remains **unique** on
  `(discovery_handler_id, upstream_url)` for both fresh installations. The old
  openGauss index was non-unique despite its name. The proxy API-key mapping
  unique index, primary keys and lookup indexes are also retained.
- PostgreSQL's `tag.ext` non-null constraint is retained; Admin always builds
  extension JSON when creating a tag. Discovery columns use PostgreSQL's order;
  runtime mappers name their columns explicitly.
- The keyAuth, tcp, basicAuth and mock resource menus and admin grants missing
  from the old openGauss copy are retained. API-key permission IDs use the
  PostgreSQL values; their role/resource grants were identical. Permissions
  targeting absent resources are removed. The aiRequestTransformer edit
  permission retains the correct spelling.
- The openGauss custom rule-page defaults for paramMapping and modifyResponse,
  the request rule-page handle and Kafka selector security handles are retained.
  Dubbo uses one `loadBalance` rule handle with the supported spelling and
  `random` default. Cache retains `cacheType` and a single `timeoutSeconds`
  handle; Elasticsearch retains the required index-name field. Dubbo retry/
  timeout handle IDs, aiProxy fallback ordering and timestamp precision use
  PostgreSQL's values.
- basicAuth keeps valid escaped JSON. loggingKafka uses `bootstrapServer`, and
  aiProxy retains its fallback defaults without the obsolete `prompt` field.
  Their default-namespace configs match the plugin defaults, as do the custom
  rule-page defaults. All seeded plugins retain their default-namespace relation.

Native fresh-install checks inspect columns/nullability, primary keys, indexes
and uniqueness; reject orphan relations, duplicate natural keys and invalid JSON;
and require admin grants and default-namespace relations. They also attempt a
duplicate discovery upstream and require the intended unique index to reject it.
Historical upgrade scripts remain separate and unchanged, and upgrade validation
still loads the original openGauss schema from the immutable released baseline.
