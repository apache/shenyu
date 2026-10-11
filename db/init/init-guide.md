# Initialization Guide

The directories contain database initialization scripts for MySQL, OceanBase,
Oracle, PostgreSQL, and openGauss.

## To Shenyu Users

- mysql/schema.sql

  > The file is the Mysql initialization script.

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

