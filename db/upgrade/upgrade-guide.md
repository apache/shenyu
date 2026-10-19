# Upgrade Guide

> the file `*-upgrade-*-mysql.sql`、`*-upgrade-*-pg.sql` works for MySQL and PostgreSQL.

## To Shenyu Users

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
