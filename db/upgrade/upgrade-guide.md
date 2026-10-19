# Upgrade Guide

> the file `*-upgrade-*-mysql.sql`、`*-upgrade-*-pg.sql` works for MySQL and PostgreSQL.

## To Shenyu Users

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

### Data permission uniqueness in 2.7.2

Stop Admin instances before running the `2.7.1-upgrade-2.7.2-<dialect>.sql`
script, then restart them after it completes. Run the version upgrade script
once. It removes duplicate `data_permission` rows with the same
`(user_id, data_type, data_id)`, retaining the complete row with the smallest
existing `id`, and creates the unique `idx_data_permission_user_type` index.
Each distinct grant is preserved. Fresh installations create the same unique
index directly.

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
