# Upgrade Guide

> the file `*-upgrade-*-mysql.sql`、`*-upgrade-*-pg.sql` works for MySQL and PostgreSQL.

## To Shenyu Users

### Optional AI token reservations in 2.7.2

The `2.7.1-upgrade-2.7.2-<dialect>.sql` scripts add the `reservationEnabled`
and `reservationTokens` rule form fields for `aiTokenLimiter`, plus the boolean
dictionary choices. Fresh installations include the same metadata. Existing
rules keep their JSON handles and remain in the default completed-usage mode.
To opt in, explicitly enable reservations and choose a positive estimated
completion-token amount per request, no greater than the rule's token budget.
See the AI token limiter module README for the unknown-usage and window policies.

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
