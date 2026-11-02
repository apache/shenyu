# Upgrade Guide

> the file `*-upgrade-*-mysql.sql`、`*-upgrade-*-pg.sql` works for MySQL and PostgreSQL.

## To Shenyu Users

### API-debug request headers in 2.7.3

Stop Admin writers and back up `mock_request_record` before applying the
`2.7.2-upgrade-2.7.3-<dialect>.sql` migration. It changes `header` from
`VARCHAR(1024)` to TEXT (MySQL/OceanBase/PostgreSQL/openGauss) or CLOB (Oracle),
preserving existing rows and header contents. Oracle copies the value to a CLOB
column before replacing the old column because a direct VARCHAR2-to-CLOB type
change is not supported. Run the complete version migration once, then deploy
the updated Admin.

The `2.7.2-upgrade-2.7.3-h2.sql` migration includes the persistent-H2 change
(fresh H2 databases create CLOB directly). The header-specific statement is:

```sql
ALTER TABLE mock_request_record ALTER COLUMN header CLOB;
```

All Admin mapper insert paths store an absent header as an empty value. MySQL 5.7
does not allow a literal default for TEXT, so the column has no default and Admin
normalizes the value explicitly. Custom SQL writers must supply a header value. MySQL/OceanBase TEXT retains the
native 65,535-byte limit. Oracle reads/writes use complete CLOB values and equality
filters compare the whole value, including content beyond the first 4,000 characters.


### `loged` column renamed to `logged` in 2.7.3

Running the `2.7.2-upgrade-2.7.3-<dialect>.sql` script is mandatory for every
existing deployment: it renames the misspelled `loged` column to `logged` on
both the `selector` and `rule` tables, and the new Admin expects `logged`.
Upgrade order matters — the field is part of the Admin-to-bootstrap sync
contract. Upgrade all bootstrap instances before, or together with, Admin:
an Admin emitting `logged` is not understood by older bootstraps. Stop Admin
instances before running the script, then restart them after it completes.
Fresh installations create the renamed columns directly.
The MySQL script uses `ALTER TABLE ... RENAME COLUMN`, which requires
MySQL 8.0 or later; on MySQL 5.7 apply the same rename with the equivalent
`ALTER TABLE ... CHANGE COLUMN loged logged ...` syntax instead.

- 2.7.2-upgrade-2.7.3-mysql.sql

- 2.7.2-upgrade-2.7.3-ob.sql

- 2.7.2-upgrade-2.7.3-og.sql

- 2.7.2-upgrade-2.7.3-oracle.sql

- 2.7.2-upgrade-2.7.3-pg.sql

  > this file is the Shenyu upgrade sql from v2.7.2 to v2.7.3

### Tracing plugin registered in the control plane in 2.7.3

The `2.7.2-upgrade-2.7.3-<dialect>.sql` scripts also register the `tracing`
plugin, which shipped in 2.7.2 without a control plane entry. The migration inserts
the plugin, its relation to the default namespace, the dashboard menu entry with
its nine button permissions, and the admin grants for them. The plugin and the
relation are inserted **enabled**: a relation registered disabled would push a
disable event through the sync channel and remove the plugin from the gateway
chain, so an installation that already traces through `shenyu.tracing.enabled`
would silently stop tracing on upgrade.

Tracing now has two switches with different meanings:

- `shenyu.tracing.enabled` decides whether the plugin is installed at all. The
  starter creates no plugin bean while it is false, so the dashboard entry exists
  but nothing runs on the chain.
- The tracing plugin relation is the switch the gateway sync channel reads.
  Disabling it in the dashboard removes the installed plugin from the plugin
  chain without a restart, and enabling it puts it back. The relation is stored per
  namespace and the migration creates the one of the default namespace; a
  namespace created later copies `enabled` and `sort` from the plugin row, which is
  why that row stays enabled.

Note that the gateway always starts with the plugin beans on its chain, and only a
disable or a delete event takes one off it. A deployment whose synced data carries
no tracing relation therefore keeps tracing as well. To stop tracing, disable the
relation instead of deleting it.

Everything below `shenyu.tracing.*` (service name, sampler, resource attributes,
OTLP endpoint) still needs a restart, because the tracer provider and the exporter
are created when it starts. The plugin has no selectors, rules or handles and
keeps the local configuration model of `metrics`, so its dashboard page shows no
configuration form and any selector or rule created for it would not be read.

### Instance identity uniqueness in 2.7.3

The `2.7.2-upgrade-2.7.3-<dialect>.sql` scripts also enforce one
`instance_info` row per `(namespace_id, instance_ip, instance_port, instance_type)`.
Stop **all** Admin writers and back up `instance_info` before running the
one-time migration. Existing duplicate rows are deleted, retaining the complete
row with the greatest heartbeat timestamp, then greatest `date_updated`,
`date_created`, and finally `id`. Distinct identities and namespaces are preserved;
there are no references from gateway configuration to these diagnostic row IDs.
Rows upgraded from 2.7.1 initially have heartbeat `0`, so timestamps and ID decide
the winner until a heartbeat has been persisted.

Apply the schema migration before starting the updated Admin. Fresh installations
create `uk_instance_info_identity` directly. An existing persistent H2 database
requires `2.7.2-upgrade-2.7.3-h2.sql`; rerunning `CREATE TABLE IF NOT EXISTS` does
not upgrade an existing table. H2 databases predating 2.7.2 must first have the
`last_heart_beat_time BIGINT DEFAULT 0 NOT NULL` column added.

The unique key uses all four columns without prefix truncation (434 characters,
at most 1,736 bytes with MySQL utf8mb4). MySQL installations using a legacy
767-byte InnoDB index limit must enable the modern large-prefix/DYNAMIC format
or upgrade before applying it. Prefix indexes would conflate distinct identities.

Concurrent insert conflicts reload and update the canonical row. Updates accept
only an equal or newer heartbeat; equal timestamps still allow liveness state
changes without a fresh heartbeat. Zero or older heartbeats cannot overwrite a
positive newer observation. This orders observations by their reported timestamps;
it does not establish a global clock or break ties between simultaneous states.
Instance diagnostic writes run independently of caller transactions so a duplicate
insert does not leave PostgreSQL conflict recovery in an aborted transaction.

### Data permission uniqueness in 2.7.2

Stop Admin instances before running the `2.7.1-upgrade-2.7.2-<dialect>.sql`
script, then restart them after it completes. Run the version upgrade script
once. It removes duplicate `data_permission` rows with the same
`(user_id, data_type, data_id)`, retaining the complete row with the smallest
existing `id`, and creates the unique `idx_data_permission_user_type` index.
Each distinct grant is preserved. Fresh installations create the same unique
index directly.

### Durable instance heartbeat timestamps in 2.7.2

The same `2.7.1-upgrade-2.7.2-<dialect>.sql` script adds a
`last_heart_beat_time` column to the `instance_info` table, so the last
reported heartbeat survives Admin restarts and master handover. Existing rows
are backfilled with `0`, which means "no durable observation yet"; the real
timestamp is stored on the next heartbeat report.

- 2.7.1-upgrade-2.7.2-mysql.sql

- 2.7.1-upgrade-2.7.2-ob.sql

- 2.7.1-upgrade-2.7.2-og.sql

- 2.7.1-upgrade-2.7.2-oracle.sql

- 2.7.1-upgrade-2.7.2-pg.sql

  > this file is the Shenyu upgrade sql from v2.7.1 to v2.7.2

- 2.7.0-upgrade-2.7.1-mysql.sql

- 2.7.0-upgrade-2.7.1-og.sql

- 2.7.0-upgrade-2.7.1-oracle.sql

- 2.7.0-upgrade-2.7.1-pg.sql

  > this file is the Shenyu upgrade sql from v2.7.0 to v2.7.1

- 2.6.1-upgrade-2.7.0-mysql.sql

- 2.6.1-upgrade-2.7.0-og.sql

- 2.6.1-upgrade-2.7.0-oracle.sql

- 2.6.1-upgrade-2.7.0-pg.sql

  > this file is the Shenyu upgrade sql from v2.6.1 to v2.7.0

- 2.6.0-upgrade-2.6.1-mysql.sql

- 2.6.0-upgrade-2.6.1-og.sql

- 2.6.0-upgrade-2.6.1-oracle.sql

- 2.6.0-upgrade-2.6.1-pg.sql

  > this file is the Shenyu upgrade sql from v2.6.0 to v2.6.1

- 2.5.1-upgrade-2.6.0-mysql.sql

- 2.5.1-upgrade-2.6.0-og.sql

- 2.5.1-upgrade-2.6.0-oracle.sql

- 2.5.1-upgrade-2.6.0-pg.sql

  > this file is the Shenyu upgrade sql from v2.5.1 to v2.6.0

- 2.5.0-upgrade-2.5.1-mysql.sql

- 2.5.0-upgrade-2.5.1-oracle.sql

- 2.5.0-upgrade-2.5.1-pg.sql

  > this file is the Shenyu upgrade sql from v2.5.0 to v2.5.1

- 2.4.3-upgrade-2.5.0-mysql.sql

- 2.4.3-upgrade-2.5.0-pg.sql

  > this file is the Shenyu upgrade sql from v2.4.3 to v2.5.0

- 2.4.2-upgrade-2.4.3-mysql.sql

- 2.4.2-upgrade-2.4.3-pg.sql

  > this file is the Shenyu upgrade sql from v2.4.2 to v2.4.3

- 2.4.1-upgrade-2.4.2-mysql.sql

- 2.4.1-upgrade-2.4.2-pg.sql

  > this file is the Shenyu upgrade sql from v2.4.1 to v2.4.2
