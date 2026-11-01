# Container runtime guide

This document describes the container contract of the `shenyu-admin` and
`shenyu-bootstrap` distribution images built from `shenyu-dist`. It covers the
base-image policy, the runtime filesystem/signal contract, JVM and memory
budgets, the Kubernetes defaults used for production rollouts, and the local and
CI verification steps.

Related: [#8072](https://github.com/apache/shenyu/issues/8072) (container
distribution hardening), [#5273](https://github.com/apache/shenyu/issues/5273)
(Kubernetes controller follow-up), [#7649](https://github.com/apache/shenyu/issues/7649)
(minimal HTTP distribution).

For incident investigation and workload-based JVM option evaluation, see the
[Bootstrap GC and memory diagnostics guide](bootstrap-gc-memory.md)
([中文](bootstrap-gc-memory-zh.md)).

## 1. Images

| Image | Dockerfile | Runtime port | Health endpoint |
|---|---|---|---|
| `apache/shenyu-admin` | `shenyu-dist/shenyu-admin-dist/docker/Dockerfile` | `9095` | `/actuator/health` |
| `apache/shenyu-bootstrap` | `shenyu-dist/shenyu-bootstrap-dist/docker/Dockerfile` | `9195` | `/actuator/health` |

Both images share the same runtime contract:

| Property | Value |
|---|---|
| Runtime base | `amazoncorretto:17.0.21-alpine3.24` (JDK 17, Alpine 3.24) |
| Prepare stage | `alpine:3.24` (only unpacks the distribution tarball) |
| Supported platforms | `linux/amd64`, `linux/arm64` (published multi-arch by `Makefile` / `docker-publish*.yml`) |
| User | `10001:10001` (`shenyu:shenyu` in `/etc/passwd`) |
| Signals | `STOPSIGNAL SIGTERM`, JVM is PID 1 |
| Writable paths | `/tmp` only (by default) |
| In-image tooling | `wget` in both images; the admin image also ships `curl` |

### Base-image update policy

The JDK patch version and the Alpine generation are pinned on purpose. To keep
the images on maintained bases:

1. Re-check `amazoncorretto` and `alpine` tags at least once per quarter, and
   immediately after a Corretto security release.
2. Bump **both** Dockerfiles and the table above in the same pull request; the
   admin and bootstrap images must not drift onto different JDK patches.
3. Run `bash script/docker-image-smoke.sh` before opening the pull request.
4. If a base change is not backward compatible (for example a new Alpine
   generation drops a package), record the change in the release notes.

Do not use floating tags (`17-alpine`, `latest`) in the Dockerfiles. The
`-alpine3.24` and `17.0.21` parts are the compatibility and security pins; the
patch version is what scanners and release notes track.

### Java runtime

The images ship a full JDK on purpose: `jcmd`, `jstack`, `jmap` and `jstat` are
available when an operator execs into a running container. The distribution
tarball also contains `bin/start.sh` and `bin/stop.sh`, but those helpers are
bash scripts intended for bare-metal tarball installations and are **not**
runnable inside the images (there is no `bash` interpreter); the container
entrypoint starts the application instead. Switching to a JRE-only or
`-headless` base is a separate decision; native-image compilation is explicitly
**not** part of this contract (see #8072 boundaries).

## 2. Runtime contract

### 2.1 Filesystem and read-only root filesystems

The application writes logs to stdout only (`conf/logback.xml` uses an async
console appender), so the root filesystem can be mounted read-only:

```bash
docker run --read-only --tmpfs /tmp:rw,size=64m \
  -p 9195:9195 \
  apache/shenyu-bootstrap:<tag>
```

| Path | Writable? | Purpose | Production guidance |
|---|---|---|---|
| `/tmp` | required | JVM temporary files, HTTP client temp buffers, `java.io.tmpdir` | mount an `emptyDir` or `tmpfs`; size it to at least 64 MiB |
| `/opt/shenyu-{admin,bootstrap}/conf` | no | image configuration | mount ConfigMap files instead of writing into the image |
| `/opt/shenyu-{admin,bootstrap}/conf-ext` | read-only | optional override directory | mount a ConfigMap/Secret directory; see 2.2 |
| `/opt/shenyu-{admin,bootstrap}/logs` | no | unused by default (console logging) | only needed if you replace `logback.xml` with a file appender |

If you enable file logging or a file-backed database (for example an H2 or
SQLite file instead of the default in-memory/MySQL/PostgreSQL setup), mount a
writable volume for that path; the image will otherwise fail to start with a
read-only root filesystem.

### 2.2 `conf-ext` overrides

The entrypoint supports an optional `conf-ext` directory next to `conf`:

* writable `conf` directory: `conf-ext/*` is copied over `conf` (legacy
  behavior); if the copy only partially succeeds, the entrypoint logs a `WARN`
  and falls back to the classpath override so no override is silently dropped;
* read-only root filesystem: `conf-ext` is prepended to the classpath instead,
  so overrides still win without writing to the image.

Both paths are logged by the entrypoint at startup. Prefer mounting individual
files (`application.yml`, `application-<profile>.yml`) with `subPath`, as the
e2e manifests do, when you only need to replace a few files.

### 2.3 Signals, shutdown and exit codes

`entrypoint.sh` ends with `exec java ...`, so the JVM is PID 1 and receives the
container signal directly; the image also declares `STOPSIGNAL SIGTERM`.

| Event | Behavior | Exit code |
|---|---|---|
| `docker stop` / Kubernetes pod termination (SIGTERM) | Spring Boot shutdown hooks run, Netty event loops and data-sync clients close; the JVM then terminates on the signal | `143` (`128 + SIGTERM`) in the normal case, `0` only if the application exits during shutdown |
| Grace period exceeded, then `SIGKILL` | runtime kills the process | `137` |
| Startup failure (bad config, missing class, port in use) | JVM exits | `1` |

Both `0` and `143` indicate a graceful stop: the signal reached the JVM and the
hooks ran. `137` means the shutdown took longer than the grace period, which is
the signal to raise `terminationGracePeriodSeconds` or shorten the shutdown
hooks.

Set `terminationGracePeriodSeconds` to at least `30` for the gateway and `60`
for the admin so that the shutdown hooks can finish before the runtime sends
`SIGKILL`. The default `docker stop` grace period is 10 seconds and is too short
for a busy gateway.

### 2.4 JVM option overrides

| Image | Environment variable | Default (unchanged from previous releases) |
|---|---|---|
| bootstrap | `BOOT_JVM` | `-server -Xmx2g -Xms2g -Xmn1g -Xss512k -XX:+DisableExplicitGC -XX:LargePageSizeInBytes=128m` |
| admin | `ADMIN_JVM` | `-server -Xmx2g -Xms2g -Xmn1g -Xss328k -XX:+DisableExplicitGC -XX:LargePageSizeInBytes=128m` |

`BOOT_JVM` / `ADMIN_JVM` **replace** the default option string; they are not
appended to it. Container-aware sizing is therefore an explicit, reviewable
change per deployment (see section 3). Note that the e2e/compose fixtures pass
`-Xmx768m -Xms768m` through Kubernetes `args:` / Compose `command:`; those are
application arguments passed after the main class, not JVM flags — set JVM
options through `BOOT_JVM`/`ADMIN_JVM` instead.

## 3. JVM and container memory budgets

The default `-Xmx2g` is a JVM setting, not a container-aware one: it does not
shrink or grow with the pod limit. Size the container from the JVM budget, not
the other way around.

| Component | Flag | Default / guidance |
|---|---|---|
| Java heap | `-Xmx`, `-Xms`, `-Xmn` | size from the measured live set, allocation rate and pause goals; a fixed young generation constrains G1's adaptive pause control |
| Metaspace | `-XX:MaxMetaspaceSize` | measure the actual plugin/classloader set and class-loading peaks before setting a cap |
| Code cache | `-XX:ReservedCodeCacheSize` | distinguish reserved address space from committed/resident memory |
| Direct/native buffers | `-XX:MaxDirectMemorySize` | measure active/pooled buffers under peak body sizes, streaming and retries; this flag does not cap all native allocations |
| Thread stacks | `-Xss` | gateway default 512 KiB; every request/selector thread adds a stack |
| GC and JVM overhead | — | allow for collector structures, native libraries, diagnostics and operating headroom based on observed peaks |
| Connection pools | plugin config | `maxConnections` per upstream client, database pool sizes; these consume heap *and* file descriptors |

The following `2Gi` deployment illustrates the override syntax only. Its values
are not a validated recommendation for every plugin set or workload; use the
[diagnostic checklist and workload matrix](bootstrap-gc-memory.md) before
adopting them. Percentage-based heap sizing does not automatically budget
direct memory, metaspace or thread stacks.

```yaml
env:
  - name: BOOT_JVM
    value: >-
      -XX:MaxRAMPercentage=70 -XX:InitialRAMPercentage=40
      -XX:MaxMetaspaceSize=256m -XX:MaxDirectMemorySize=256m
      -Xss512k -XX:+ExitOnOutOfMemoryError
resources:
  requests:
    memory: 2Gi
  limits:
    memory: 2Gi
```

Validate the complete budget against RSS/cgroup usage, including temporary
storage when `/tmp` is a tmpfs. `-Xmx` is not a total process limit. A direct
memory or metaspace cap can fail allocations even with heap space available;
there is no universal heap percentage or native-memory allowance. Account for
thread count when evaluating `-Xss`, and test the deployment's OOM/restart
policy separately from its throughput and latency goals.

## 4. Kubernetes production defaults

The following snippets are starting points, not universal values. They encode
the runtime contract above: non-root, read-only root filesystem, an `emptyDir`
for `/tmp`, probes on the Spring Boot health endpoint, a disruption budget and
topology spread for multi-zone clusters.

### 4.1 Bootstrap

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: shenyu-bootstrap
spec:
  replicas: 2
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxSurge: 1
      maxUnavailable: 0
  selector:
    matchLabels:
      app: shenyu-bootstrap
  template:
    metadata:
      labels:
        app: shenyu-bootstrap
    spec:
      terminationGracePeriodSeconds: 60
      securityContext:
        runAsNonRoot: true
        runAsUser: 10001
        runAsGroup: 10001
        fsGroup: 10001
        seccompProfile:
          type: RuntimeDefault
      topologySpreadConstraints:
        - maxSkew: 1
          topologyKey: topology.kubernetes.io/zone
          whenUnsatisfiable: ScheduleAnyway
          labelSelector:
            matchLabels:
              app: shenyu-bootstrap
      containers:
        - name: shenyu-bootstrap
          image: apache/shenyu-bootstrap:<pinned-tag-or-digest>
          imagePullPolicy: IfNotPresent
          ports:
            - name: http
              containerPort: 9195
          securityContext:
            allowPrivilegeEscalation: false
            readOnlyRootFilesystem: true
            capabilities:
              drop: ["ALL"]
          resources:
            requests:
              cpu: 500m
              memory: 2Gi
            limits:
              cpu: "2"
              memory: 2Gi
          env:
            # Activates the mounted application-bootstrap-sync-websocket.yml; without
            # it Spring Boot ignores the file and the gateway keeps the packaged
            # ws://localhost:9095 default.
            - name: SPRING_PROFILES_ACTIVE
              value: bootstrap-sync-websocket
            - name: BOOT_JVM
              value: >-
                -XX:MaxRAMPercentage=70 -XX:InitialRAMPercentage=40
                -XX:MaxMetaspaceSize=256m -XX:MaxDirectMemorySize=256m
                -Xss512k -XX:+ExitOnOutOfMemoryError
          volumeMounts:
            - name: tmp
              mountPath: /tmp
            - name: shenyu-bootstrap-application
              mountPath: /opt/shenyu-bootstrap/conf/application.yml
              subPath: shenyu-bootstrap-application.yml
            - name: shenyu-bootstrap-sync
              mountPath: /opt/shenyu-bootstrap/conf/application-bootstrap-sync-websocket.yml
              subPath: application-bootstrap-sync-websocket.yml
          startupProbe:
            httpGet:
              path: /actuator/health/readiness
              port: http
            periodSeconds: 5
            failureThreshold: 36
          readinessProbe:
            httpGet:
              path: /actuator/health/readiness
              port: http
            periodSeconds: 10
            timeoutSeconds: 3
            failureThreshold: 3
          livenessProbe:
            httpGet:
              path: /actuator/health/liveness
              port: http
            periodSeconds: 10
            timeoutSeconds: 3
            failureThreshold: 6
      volumes:
        - name: tmp
          emptyDir:
            sizeLimit: 256Mi
        - name: shenyu-bootstrap-application
          configMap:
            name: shenyu-configmap
            items:
              - key: shenyu-bootstrap-application.yml
                path: shenyu-bootstrap-application.yml
        - name: shenyu-bootstrap-sync
          configMap:
            name: shenyu-configmap
            items:
              - key: application-bootstrap-sync-websocket.yml
                path: application-bootstrap-sync-websocket.yml
---
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: shenyu-bootstrap
spec:
  maxUnavailable: 1
  selector:
    matchLabels:
      app: shenyu-bootstrap
```

The readiness group only becomes meaningful with these properties in the mounted
sync profile (`application-bootstrap-sync-websocket.yml`):

```yaml
management:
  endpoint:
    health:
      probes:
        enabled: true
      group:
        readiness:
          include: readinessState,websocketSync
        liveness:
          include: livenessState
shenyu:
  sync:
    websocket:
      initial-sync-readiness: true
      urls: ws://shenyu-admin:9095/websocket
```

With `initial-sync-readiness: true`, the `websocketSync` health indicator joins
the readiness group and the pod only reports ready after the first full config
sync; without `probes.enabled: true` the `/actuator/health/readiness` and
`/actuator/health/liveness` endpoints do not exist. `shenyu-cm.yml` in
`shenyu-e2e/shenyu-e2e-case/k8s/sync/` is the working reference.

### 4.2 Admin

The admin adds a database dependency and a heavier startup, so it uses a longer
`startupProbe` window. Keep `maxUnavailable: 0` for the admin when the database is
shared with the gateways' configuration, so configuration writes never stop
during a rollout.

Reuse the bootstrap Deployment scaffolding (pod security context, `emptyDir`
`/tmp`, `readOnlyRootFilesystem`, topology spread, `terminationGracePeriodSeconds`)
and replace everything below `containers:` with this block:

```yaml
        - name: shenyu-admin
          image: apache/shenyu-admin:<pinned-tag-or-digest>
          imagePullPolicy: IfNotPresent
          ports:
            - name: http
              containerPort: 9095
          securityContext:
            allowPrivilegeEscalation: false
            readOnlyRootFilesystem: true
            capabilities:
              drop: ["ALL"]
          resources:
            requests:
              cpu: "1"
              memory: 2Gi
            limits:
              cpu: "2"
              memory: 2Gi
          env:
            # Required by current admin releases: multiple admin instances must
            # share the same JWT signing key or tokens fail verification randomly.
            - name: SHENYU_JWT_SECRETKEY
              valueFrom:
                secretKeyRef:
                  name: shenyu-admin-secrets
                  key: jwt-secret-key
            # Selects the packaged database profile (h2 is the default and is
            # in-memory only; use mysql/pg/oracle/ob/og in production).
            - name: SPRING_PROFILES_ACTIVE
              value: mysql
            - name: ADMIN_JVM
              value: >-
                -XX:MaxRAMPercentage=70 -XX:InitialRAMPercentage=40
                -XX:MaxMetaspaceSize=384m -XX:MaxDirectMemorySize=256m
                -Xss328k -XX:+ExitOnOutOfMemoryError
          volumeMounts:
            - name: tmp
              mountPath: /tmp
            - name: shenyu-admin-application
              mountPath: /opt/shenyu-admin/conf/application.yml
              subPath: shenyu-admin-application.yml
          startupProbe:
            httpGet:
              path: /actuator/health
              port: http
            periodSeconds: 5
            failureThreshold: 60
          readinessProbe:
            httpGet:
              path: /actuator/health
              port: http
            periodSeconds: 10
            timeoutSeconds: 3
            failureThreshold: 3
          livenessProbe:
            httpGet:
              path: /actuator/health
              port: http
            periodSeconds: 15
            timeoutSeconds: 3
            failureThreshold: 6
```

and the bootstrap `volumes:` block with:

```yaml
      volumes:
        - name: tmp
          emptyDir:
            sizeLimit: 256Mi
        - name: shenyu-admin-application
          configMap:
            name: shenyu-admin-configmap
            items:
              - key: shenyu-admin-application.yml
                path: shenyu-admin-application.yml
```

The admin listens on `9095` and must not mount the bootstrap config files. Keep
its probes on the aggregate `/actuator/health`, which includes the database
health indicator: the pod should not be ready while its database is unreachable.

Without `shenyu.jwt.secret-key` (or `SHENYU_JWT_SECRETKEY`) the admin fails
fast at startup instead of booting with a per-instance random key, which would
cause intermittent `401`s behind a load balancer.

### 4.3 Probe notes

* `startupProbe` exists because both applications need to load plugins and
  establish the data-sync channel before they are ready; a plain liveness probe
  would restart a healthy, still-starting pod.
* Bootstrap probes target `/actuator/health/readiness` and
  `/actuator/health/liveness`; those endpoints require the two properties shown
  in 4.1. Admin probes use the aggregate `/actuator/health` because it includes
  the database health indicator, which is the desired readiness signal for the
  control plane; a pod that cannot reach its database should not serve dashboard
  traffic.
* Keep probe timeouts at 3 seconds or less; a slow probe itself becomes a
  liveness failure during GC pauses.
* The image declares a Docker `HEALTHCHECK` (`/actuator/health`). It is
  intentionally not a replacement for Kubernetes probes: `HEALTHCHECK` has no
  readiness semantics and Kubernetes ignores it.

## 5. Upgrade and rollback

The images are stateless: configuration comes from ConfigMaps/`conf-ext`,
secrets and environment variables; the admin stores state in its database. That
makes rollback a matter of repointing the image tag.

1. Pin releases by digest (`image: apache/shenyu-bootstrap@sha256:...`) or by an
   immutable version tag; never deploy `latest` in production.
2. Roll out with `maxSurge: 1`, `maxUnavailable: 0` and a
   `PodDisruptionBudget`. New pods must pass the readiness probe before old pods
   are removed.
3. Keep the previous tag available for the rollback window and roll back by
   reverting to it. Configuration lives in ConfigMaps, Secrets and `conf-ext`,
   not in the image, so the previous image reads the same configuration; the
   smoke test restarts the image under a different tag with the same mounted
   configuration to verify that invariant, but a cross-version rollback against
   a previously published tag remains a manual release step (and a deliberate
   follow-up for automated coverage).
4. Database schema changes belong to the admin release process; read the
   release notes before rolling the admin back across a schema version.
5. Do not change the base image and the application version in the same
   deployment if you can avoid it: it makes bisecting an incident harder.
6. Roll back the whole pod template, not just the image tag. The hardened
   manifests assume this contract (`readOnlyRootFilesystem: true`, numeric UID
   `10001`, `/actuator/health/readiness` and `/actuator/health/liveness`);
   images published before this change run as root, write into `conf`, and do
   not expose the readiness groups unless the mounted profile enables
   `management.endpoint.health.probes.enabled`. Swapping only the image tag in
   the new manifest can therefore CrashLoop or never become ready.
   `kubectl rollout undo` reverts the whole ReplicaSet template (image,
   security context and probes together) and is the safe path; otherwise keep
   the previous Deployment/StatefulSet manifest next to the previous tag.
   `script/docker-image-smoke.sh` can verify the image side of this on demand
   with `SHENYU_SMOKE_PREVIOUS_TAG=<previous-release>` (see section 8): it pulls
   the previous image and starts it with the same `conf-ext` mount under that
   release's own (writable, non-read-only) contract.

## 6. Baseline measurements

Record these values in the release notes whenever the base image or distribution
changes:

```bash
# image size (uncompressed bytes) and digest (falls back to the local image ID
# for a tag that was never pushed)
docker image inspect --format '{{.Size}} {{if .RepoDigests}}{{index .RepoDigests 0}}{{else}}{{.Id}}{{end}}' apache/shenyu-bootstrap:<tag>

# time to healthy
start=$(date +%s)
docker run --rm -d --read-only --tmpfs /tmp:rw,size=64m -p 9195:9195 \
  apache/shenyu-bootstrap:<tag>
until wget -q -O /dev/null http://127.0.0.1:9195/actuator/health; do sleep 1; done
echo "healthy after $(( $(date +%s) - start ))s"

# steady-state memory after the gateway has served traffic
docker stats --no-stream <container>
```

Reference numbers for `2.7.2-SNAPSHOT` on `linux/amd64` (Docker 29, 4 vCPU,
default H2 admin profile, no `conf-ext` overrides). Image sizes are the
`docker image inspect --format '{{.Size}}'` values (the same number `docker image
ls` shows; it sums the uncompressed base layers plus the distribution payload,
so registry-compressed sizes are smaller). The time-to-healthy values come from
`SHENYU_SMOKE_REAL_DIST=1 script/docker-image-smoke.sh`; the startup and RSS
values come from a separate run that uses the default `BOOT_JVM`/`ADMIN_JVM`
option strings from the image and leaves the container idle after startup:

| Metric | bootstrap | admin |
|---|---|---|
| image size on disk (uncompressed `docker image inspect .Size`) | 745 MB (~711 MiB) | 751 MB (~716 MiB) |
| time to "Started Shenyu*" | ~7 s | ~15 s |
| time to `/actuator/health` = `UP` | ~11 s | ~20 s |
| idle RSS with the default JVM options | ~436 MiB | ~1002 MiB |
| idle RSS with the container-aware options in section 3, 2 GiB limit | ~340 MiB | ~404 MiB |

The idle RSS figures are measured before the gateway serves any traffic (no
plugins with routes registered). Gateway memory grows with the number of
selectors/rules, plugin caches, upstream connections and in-flight requests, so
use them as a starting point for the `limits` value, not as a target.

The images include a full JDK, so most of that size is the base image; the
compressed registry size is smaller. Startup time grows with the number of
gateway plugins and with a remote database, so treat these values as a floor for
regression tracking rather than as deployment expectations.

The `script/docker-image-smoke.sh` script prints image sizes and verifies the
non-root/read-only/shutdown contract; use it together with the numbers above as
the change record.

## 7. SBOMs, scanning and signing

The image contract supports standard supply-chain tooling:

```bash
# SBOM (SPDX or CycloneDX)
syft packages docker:apache/shenyu-bootstrap:<tag> -o spdx-json > bootstrap.spdx.json
syft packages docker:apache/shenyu-admin:<tag> -o cyclonedx-json > admin.cdx.json

# vulnerability scan
trivy image --severity HIGH,CRITICAL --ignore-unfixed apache/shenyu-bootstrap:<tag>
```

Integrating scanning into CI and attaching SBOMs to releases is a follow-up on
top of #8072: image signing and provenance must go through the existing ASF
release process (the artifact must not acquire a second, parallel release
authority). Until that is agreed, generate SBOMs/scans locally or in an external
pipeline rather than publishing them as release artifacts.

## 8. Local verification

```bash
# 1. Runtime contract only: builds stub distributions, no Maven build needed
bash script/docker-image-smoke.sh
# same thing through the convenience target
make smoke-image

# 2. Full images from the real distributions
./mvnw -Prelease package -DskipTests -Drat.skip=true -Djacoco.skip=true \
  -pl shenyu-dist/shenyu-bootstrap-dist,shenyu-dist/shenyu-admin-dist -am
SHENYU_SMOKE_REAL_DIST=1 bash script/docker-image-smoke.sh

# 2b. Cross-version rollback check: starts the previous published release with
#     the same mounted configuration (pulls apache/shenyu-{bootstrap,admin}:<tag>)
SHENYU_SMOKE_PREVIOUS_TAG=2.7.1 bash script/docker-image-smoke.sh

# 3. Manual image build (also used by the Makefile)
docker build -t apache/shenyu-bootstrap:local \
  -f shenyu-dist/shenyu-bootstrap-dist/docker/Dockerfile \
  --build-arg APP_NAME=apache-shenyu-<version>-bootstrap-bin \
  shenyu-dist/shenyu-bootstrap-dist
```

On Windows, run the script from Git Bash (`C:\Program Files\Git\bin\bash.exe`) or
from WSL with Docker Desktop integration; a plain `bash` in `cmd`/PowerShell can
resolve to the WSL launcher, which cannot talk to the Windows Docker daemon. The
repository's `.gitattributes` keeps the entrypoints and scripts on LF so a
Windows checkout does not break `sh entrypoint.sh` inside the image.

The smoke test checks the following:

* both images: UID `10001`, a read-only root filesystem while `/tmp` stays
  writable, and `BOOT_JVM`/`ADMIN_JVM` applied (the stub app writes to `/tmp`
  explicitly; the real-dist run exercises the real entrypoint and JVM);
* `SIGTERM` reaches the JVM, the shutdown hook completes, and the process exits
  with `0`/`143` instead of being killed with `137`;
* real-dist mode additionally waits for `/actuator/health` = `UP` and for Docker
  to report the image `HEALTHCHECK` as `healthy`, so a broken healthcheck cannot
  ship silently;
* stub mode additionally checks the `conf-ext` paths (read-only classpath
  fallback and the legacy writable copy) and that restarting the image under a
  different tag reads the same mounted configuration;
* `SHENYU_SMOKE_PREVIOUS_TAG=<release>` additionally pulls that published
  release and verifies it starts and stops gracefully with the same mounted
  configuration (the previous image is run with its own contract: writable root
  filesystem, no fixed UID, old entrypoint).

CI runs the stub variant (`.github/workflows/docker-image-smoke.yml`) for pull
requests that touch the distribution Dockerfiles, entrypoints, assembly
descriptors, `Makefile`, this guide or the smoke script itself.

## 9. Behavior changes relative to the previous images

| Change | Impact |
|---|---|
| Runtime base moved from `eclipse-temurin:17-centos7` (CentOS 7, EOL) to `amazoncorretto:17.0.21-alpine3.24`; the admin image moved from `17.0.11-alpine3.19` to the same tag | maintained base with security patches for `linux/amd64` and `linux/arm64` |
| Bootstrap switched from glibc (CentOS 7) to musl (Alpine) | pure-Java plugins are unaffected; JNI libraries built only against glibc cannot be loaded (check the native dependencies of custom plugins before upgrading) |
| `bash` is no longer present (the bootstrap image used to ship it; the admin image was already Alpine-based) | `docker exec ... bash` and the tarball's `bin/*.sh` helpers do not work inside the container; use `sh` and the POSIX entrypoint, or `jcmd`/`jstack` for diagnostics |
| Bootstrap loses `curl` (the old CentOS 7 base shipped `curl` and GNU `wget`); both images now ship BusyBox-traditional `wget` only, the admin image also keeps `curl` | in-container scripts/healthchecks that call `curl` must switch to `wget -q -O /dev/null` (or install `curl` in a derived image); the image `HEALTHCHECK` and the compose example already use `wget` |
| Entrypoint rewritten as POSIX `sh` and always `exec`s the JVM | no bash dependency; SIGTERM is delivered to the JVM directly |
| Fixed numeric UID/GID `10001` (`USER 10001:10001`) | `runAsUser` is stable, and `runAsNonRoot: true` can be verified by the kubelet; explicitly set `runAsUser`/`runAsGroup` in manifests that previously relied on an image-local user |
| `readOnlyRootFilesystem` fully supported | mount `emptyDir`/`tmpfs` on `/tmp`; `conf-ext` falls back to a classpath override |
| `HEALTHCHECK` and `STOPSIGNAL SIGTERM` declared | Docker/Compose users get a health signal and a correct stop signal by default; `HEALTHCHECK` uses `wget`, so the admin image keeps `curl` only as an operator convenience |
