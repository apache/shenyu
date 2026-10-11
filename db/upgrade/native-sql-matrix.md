# Native SQL compatibility merge gate

The `native-sql-matrix` workflow reports a stable aggregate check named
`sql-matrix`. Maintainers can configure this check as required after validating
the workflow on their infrastructure. Adding the workflow does not change
repository branch-protection or ruleset settings.

For SQL/schema, Admin, or matrix-harness changes the gate requires:

- the offline harness regression tests;
- the real H2 mapper/service/controller list, detail/export and index tests;
- all five native database jobs (MySQL, OceanBase MySQL mode, PostgreSQL,
  openGauss, Oracle Free), each running both upgrade and fresh-install flows.

Failures, cancellations, skipped applicable jobs, missing scope output and
startup timeouts fail the gate. An unrelated change explicitly reports the
matrix as not applicable; the aggregate still runs. The matrix uses
`fail-fast: false` so one engine does not cancel the other engines.

## Native execution

Database images are digest-pinned in `.github/scripts/native-sql-matrix.py`:
MySQL 8.0, PostgreSQL 15, openGauss lite 5.0.1, OceanBase 4.3.5 LTS and Oracle
Free 23.26.3 slim. This validates these selected engine versions, not every
vendor/version combination. Images run in disposable Linux amd64 containers,
without published ports, production credentials, host mounts or privileged
mode. Each dialect gets an independent runner and a 40-minute job timeout.

The upgrade starts from the actual `v2.7.1` schema at immutable commit
`218c5634ebffb1f0ce7e8ea921b85cd5a633cde7` and executes the complete current
`2.7.1-upgrade-2.7.2-<dialect>.sql` without rewriting or filtering its SQL.
The fresh flow executes the complete current initialization schema in a new
container, not the already upgraded database. Clients abort on SQL errors;
there is no `--force`, tolerated-error list or fallback to H2.

MySQL and OceanBase in MySQL mode both initialize fresh installations from
`db/init/mysql/schema.sql`, without a compatibility overlay. OceanBase's upgrade
flow still reads its own `db/init/ob/schema.sql` **at the immutable released
baseline commit**, then executes the unchanged `*-ob.sql` upgrade script.
The historical baseline is not a current initialization file.

Both flows insert a plugin with a nonempty binary JAR including zero/high-bit
bytes. The checks execute the actual shared mapper column projections, require
the list projection to exclude `plugin_jar`, preserve all sentinel list metadata
and JAR bytes, preserve pre-upgrade row IDs in plugin/selector/rule/resource/
permission/user-role/namespace-plugin relation tables, and inspect native index
catalogs. Composite indexes only count when the requested column leads the index.
The exact new selector/plugin and permission/resource indexes must also exist.

The shared MySQL/OceanBase fresh flows additionally require all declared tables,
secondary indexes and unique constraints, reject orphan seed relations and
duplicate plugin-handle/permission/namespace-plugin natural keys, require admin
permissions for every seeded resource and default-namespace relations for every
seeded plugin, and verify the loggingKafka/aiProxy default-namespace configs.

This is not a full migration idempotency test: the existing upgrade scripts are
one-time migrations. PostgreSQL/openGauss `IF NOT EXISTS` clauses are additionally
guarded by `PluginIndexUpgradeContractTest`. Native queries exercise mapper column
projections; actual Java mapper/service/API contracts are tested separately on H2.
Retained row IDs and sentinel fields are checked, not a full content fingerprint
of every production table. No gateway request or Docker e2e coverage is claimed.

Per-engine artifacts contain flow results, durations and startup logs, including
failed attempts. Cleanup is restricted to that job's generated test containers.

## Local execution

With a Linux Docker daemon available, from the repository root:

```sh
git fetch --no-tags --depth=1 origin 218c5634ebffb1f0ce7e8ea921b85cd5a633cde7
python3 -B .github/scripts/test-native-sql-matrix.py
python3 .github/scripts/native-sql-matrix.py --dialect mysql --output /tmp/shenyu-sql-matrix/mysql
```

Use `ob`, `pg`, `og` and `oracle` for the other native jobs. The workflow is also
manually dispatchable to force all jobs even without SQL changes. Offline Python
tests and workflow lint passing alone do **not** mean the native matrix passed;
require `sql-matrix` success on the current PR head before claiming verification.

OceanBase needs at least 3 GiB of available memory during startup. If other local
containers consume that memory, use a separate Docker daemon/context for the
matrix. The runner sets `nofile=65536:65536` on the OceanBase test container to
meet its file-descriptor requirement without changing the daemon or host limits.
On an ARM desktop with amd64 emulation, set `DOCKER_DEFAULT_PLATFORM=linux/amd64`
to execute the same digest-pinned image architecture used by CI.
For Oracle on Apple Silicon, the same pinned multi-architecture image also
provides a native ARM variant (`DOCKER_DEFAULT_PLATFORM=linux/arm64`). Use it if
amd64 emulation cannot start Oracle, and record the architecture with local
results; native ARM checks do not replace the required amd64 CI gate.

Image setup follows the [OceanBase container guide](https://hub.docker.com/r/oceanbase/oceanbase-ce)
and [Oracle Free image documentation](https://github.com/gvenzl/oci-oracle-free).
