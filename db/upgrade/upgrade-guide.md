# Upgrade Guide

> the file `*-upgrade-*-mysql.sql`、`*-upgrade-*-pg.sql` works for MySQL and PostgreSQL.

## To Shenyu Users

### Agent Gateway OpenAPI import provenance in 2.7.3

The `2.7.2-upgrade-2.7.3-<dialect>.sql` scripts add the Admin-only
`agent_gateway_import` table and widen `rule.handle` to `CLOB` (H2) and
`MEDIUMTEXT` (MySQL/OceanBase); PostgreSQL/openGauss already use unlimited
`TEXT` and Oracle already uses `CLOB`. The same MySQL/OceanBase scripts widen
`operation_record_log.context` to `MEDIUMTEXT`, because a rule update snapshots
the complete handle into the operation log. Generated `mcp.httpTools` plans and
the per-rule import manifest can exceed the previous H2 `VARCHAR(1024)` and
MySQL `TEXT` (65,535-byte) limits, so existing deployments must run this
migration before calling `POST /agent-gateway/openapi/preview` or `/apply`.

Stop Admin writers and back up `rule` before applying the script. The table
stores one row per target Agent Gateway rule with all source ownership maps.
It is Admin-only bookkeeping: it is never synchronized to gateways and is not
read on the request path. Deleting a target rule does not cascade to this table
(there is no foreign key, matching the existing Admin storage style), so remove
orphan rows manually if a rule is retired and its ID will not be reused.
Fresh installations create the table and the wide column directly; an existing
persistent H2 database requires `2.7.2-upgrade-2.7.3-h2.sql`. No gateway change
is required.

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

### Plugin-specific selector and rule grants in 2.7.3

Admin now accepts a selector or rule write when the role holds either the global
grant or the grant of the plugin being written. Before 2.7.3 the server checked
only the global grant, and the plugin-specific grants only decided which buttons
the dashboard showed.

| Write | Global grant | Plugin-specific grant |
|---|---|---|
| create | `system:plugin:edit` | `plugin:<name><Selector\|Rule>:add` |
| update | `system:plugin:edit` | `plugin:<name><Selector\|Rule>:edit` |
| enable or disable | `system:plugin:disable` | `plugin:<name><Selector\|Rule>:edit` |
| delete | `system:plugin:delete` | `plugin:<name><Selector\|Rule>:delete` |

There is no SQL migration, and the global grants keep their meaning on the selector
and rule endpoints. The change widens access in one case: a role that holds a
plugin-specific grant without the matching global grant was rejected by the server
before and can now write to that plugin. Review these roles before or right after
upgrading, and remove any plugin-specific grant a role should not act on.

Two requests are stricter than before:

- Deleting an API (`DELETE /api/batch`, `system:api:delete`) removes the selectors
  and rules registered for it. That cleanup now needs the delete grant from the
  table above, and the whole deletion is rejected without it. The `system:api:*`
  permissions are not part of the default resources, so this only concerns
  deployments that created them.
- An update can no longer change the plugin of an existing selector or move a rule
  to a selector of another plugin. This applies to every user, including `admin`.

This query lists the plugin-specific write grants of every role that does not hold
all three global grants:

```sql
SELECT r.id, r.role_name, res.perms
FROM role r
JOIN permission p ON p.object_id = r.id
JOIN resource res ON res.id = p.resource_id
WHERE (res.perms LIKE 'plugin:%Selector:add' OR res.perms LIKE 'plugin:%Selector:edit'
    OR res.perms LIKE 'plugin:%Selector:delete' OR res.perms LIKE 'plugin:%Rule:add'
    OR res.perms LIKE 'plugin:%Rule:edit' OR res.perms LIKE 'plugin:%Rule:delete')
  AND (SELECT COUNT(DISTINCT g.perms)
       FROM permission gp JOIN resource g ON g.id = gp.resource_id
       WHERE gp.object_id = r.id
         AND g.perms IN ('system:plugin:edit', 'system:plugin:disable', 'system:plugin:delete')) < 3
ORDER BY r.role_name, res.perms;
```

On Oracle the table is created as `"resource"`, so write both references to it
with the quotes. A role holding all three global grants is left out because the
change does not affect it. A role holding only some of them is listed with all its
plugin-specific write grants, including any that one of its global grants already
covers, so treat the result as a list to review. A fresh installation returns no
rows.

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
