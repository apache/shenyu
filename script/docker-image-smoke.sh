#!/usr/bin/env bash

# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Container runtime smoke test for the shenyu-dist images.
#
# It validates the parts of the image contract that are easy to regress:
#   * a fixed non-root UID,
#   * startup on a read-only root filesystem with only /tmp writable,
#   * conf-ext overrides working without writing to the image,
#   * SIGTERM forwarding to the JVM and graceful shutdown,
#   * the JVM option environment override (BOOT_JVM / ADMIN_JVM),
#   * restarting the image with the same mounted configuration (rollback mounts).
#
# By default the script builds throw-away stub distributions, so it does not need
# a Maven build. Set SHENYU_SMOKE_REAL_DIST=1 to smoke the tarballs produced by:
#   ./mvnw -Prelease package -pl shenyu-dist/shenyu-bootstrap-dist,shenyu-dist/shenyu-admin-dist -am
#
# Requirements: bash, docker, javac/jar (17+), tar.
#
# Optional environment variables:
#   SHENYU_SMOKE_REAL_DIST=1      smoke the real tarballs instead of stubs
#   SHENYU_SMOKE_BASE_IMAGE       expected runtime base (default: the pinned tag)
#   SHENYU_SMOKE_PREPARE_IMAGE    expected prepare-stage base
#   SHENYU_SMOKE_UID              expected container UID (default 10001)
#   SHENYU_SMOKE_JVM_ARGS         JVM options used inside the smoke containers
#   SHENYU_SMOKE_IMAGE_PREFIX     image name prefix (default shenyu-smoke)
#   SHENYU_SMOKE_PREVIOUS_TAG     opt-in: also verify a previous published release
#                                 (e.g. 2.7.1) still starts with the same mounted
#                                 configuration (pulls the image from the registry)
#   SHENYU_SMOKE_PREVIOUS_IMAGE_PREFIX
#                                 registry prefix for previous images
#                                 (default apache/shenyu)

set -euo pipefail

# Git Bash/MSYS would rewrite the container-side paths passed to docker.
export MSYS_NO_PATHCONV=1

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
readonly REPO_ROOT

BASE_IMAGE=${SHENYU_SMOKE_BASE_IMAGE:-amazoncorretto:17.0.21-alpine3.24}
readonly BASE_IMAGE
PREPARE_IMAGE=${SHENYU_SMOKE_PREPARE_IMAGE:-alpine:3.24}
readonly PREPARE_IMAGE
USE_REAL_DIST=${SHENYU_SMOKE_REAL_DIST:-0}
readonly USE_REAL_DIST
EXPECTED_UID=${SHENYU_SMOKE_UID:-10001}
readonly EXPECTED_UID
JVM_ARGS_OVERRIDE=${SHENYU_SMOKE_JVM_ARGS:-}
readonly JVM_ARGS_OVERRIDE
PREVIOUS_TAG=${SHENYU_SMOKE_PREVIOUS_TAG:-}
readonly PREVIOUS_TAG
PREVIOUS_IMAGE_PREFIX=${SHENYU_SMOKE_PREVIOUS_IMAGE_PREFIX:-apache/shenyu}
readonly PREVIOUS_IMAGE_PREFIX

WORK_DIR=$(mktemp -d)
readonly WORK_DIR
CONTAINERS=()

log() {
    # Progress goes to stderr so that command substitutions can only capture
    # values (image tags, context paths).
    printf '[docker-image-smoke] %s\n' "$*" >&2
}

fail() {
    printf '[docker-image-smoke] ERROR: %s\n' "$*" >&2
    exit 1
}

cleanup() {
    local name
    for name in "${CONTAINERS[@]:-}"; do
        docker rm -f "${name}" >/dev/null 2>&1 || true
    done
    rm -rf "${WORK_DIR}"
}
trap cleanup EXIT

# Git Bash/MSYS rewrites arguments that look like Linux paths. Hand docker and
# the host JDK the native representation of host paths; container paths stay as
# they are because MSYS_NO_PATHCONV is exported above.
to_native_path() {
    if command -v cygpath >/dev/null 2>&1; then
        cygpath -m "$1"
    else
        printf '%s\n' "$1"
    fi
}

# GNU tar treats "C:/..." as a remote host spec; on Git Bash the archive paths
# must be passed in the MSYS form instead.
to_tar_path() {
    if command -v cygpath >/dev/null 2>&1; then
        cygpath -u "$1"
    else
        printf '%s\n' "$1"
    fi
}

require_tools() {
    local tool tools="docker tar"
    # javac/jar are only needed to synthesize the stub distributions.
    if [ "${USE_REAL_DIST}" = "0" ]; then
        tools="${tools} javac jar"
    fi
    for tool in ${tools}; do
        command -v "${tool}" >/dev/null 2>&1 || fail "required tool not found: ${tool}"
    done
    docker info >/dev/null 2>&1 || fail "docker daemon is not available"
}

repo_dist_dir() {
    printf '%s/shenyu-dist/shenyu-%s-dist' "${REPO_ROOT}" "$1"
}

dist_mount_path() {
    case "$1" in
        bootstrap) printf '/opt/shenyu-bootstrap' ;;
        admin) printf '/opt/shenyu-admin' ;;
        *) fail "unknown distribution kind: $1" ;;
    esac
}

write_stub_source() {
    local package="$1" class="$2" out_dir="$3" src_dir
    src_dir="${out_dir}/${package//.//}"
    mkdir -p "${src_dir}"
    cat >"${src_dir}/${class}.java" <<EOF
package ${package};

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.CountDownLatch;

public final class ${class} {

    private ${class}() {
    }

    public static void main(final String[] args) throws Exception {
        Runtime.getRuntime().addShutdownHook(new Thread(() -> System.out.println("SHUTDOWN_HOOK_OK")));
        Path temp = Files.createTempFile("shenyu-smoke", ".tmp");
        Files.writeString(temp, "ok", StandardCharsets.UTF_8);
        Files.deleteIfExists(temp);
        System.out.println("TMP_OK");
        try (InputStream marker = ${class}.class.getClassLoader().getResourceAsStream("stub.marker")) {
            if (marker != null) {
                System.out.println("CONF_OVERRIDE_OK:" + new String(marker.readAllBytes(), StandardCharsets.UTF_8).trim());
            }
        }
        System.out.println("READY");
        System.out.flush();
        new CountDownLatch(1).await();
    }
}
EOF
}

copy_docker_context_files() {
    local kind="$1" context_dir="$2"
    mkdir -p "${context_dir}/docker" "${context_dir}/target"
    cp "$(repo_dist_dir "${kind}")/docker/Dockerfile" "${context_dir}/docker/Dockerfile"
    cp "$(repo_dist_dir "${kind}")/docker/entrypoint.sh" "${context_dir}/docker/entrypoint.sh"
    cp "$(repo_dist_dir "${kind}")/docker/logback.xml" "${context_dir}/docker/logback.xml"
}

build_stub_context() {
    local kind="$1" package="$2" class="$3"
    local context_dir app_name class_dir
    context_dir="${WORK_DIR}/context-${kind}"
    app_name="apache-shenyu-smoke-${kind}-bin"
    class_dir="${WORK_DIR}/classes-${kind}"

    copy_docker_context_files "${kind}" "${context_dir}"
    rm -rf "${WORK_DIR}/src-${kind}" "${class_dir}"
    write_stub_source "${package}" "${class}" "${WORK_DIR}/src-${kind}"
    mkdir -p "${class_dir}"
    # The stub runs inside the image JVM, so target its bytecode level explicitly.
    javac --release 17 -d "$(to_native_path "${class_dir}")" \
        "$(to_native_path "${WORK_DIR}/src-${kind}/${package//.//}/${class}.java")"
    mkdir -p "${WORK_DIR}/unpack-${kind}/${app_name}/lib" "${WORK_DIR}/unpack-${kind}/${app_name}/conf"
    (cd "$(to_native_path "${class_dir}")" && jar --create --file "$(to_native_path "${WORK_DIR}/unpack-${kind}/${app_name}/lib/shenyu-smoke.jar")" .)
    cp "$(repo_dist_dir "${kind}")/docker/logback.xml" "${WORK_DIR}/unpack-${kind}/${app_name}/conf/logback.xml"
    tar -czf "$(to_tar_path "${context_dir}/target/${app_name}.tar.gz")" -C "$(to_tar_path "${WORK_DIR}/unpack-${kind}")" "${app_name}"
    [ -s "${context_dir}/target/${app_name}.tar.gz" ] || fail "failed to create the stub distribution tarball for ${kind}"
    printf '%s\n' "${context_dir}"
}

build_real_context() {
    local kind="$1" context_dir tarball="" candidate
    context_dir="${WORK_DIR}/context-${kind}"
    for candidate in "$(repo_dist_dir "${kind}")/target"/*-"${kind}"-bin.tar.gz; do
        if [ -f "${candidate}" ]; then
            tarball="${candidate}"
            break
        fi
    done
    [ -n "${tarball}" ] || fail "SHENYU_SMOKE_REAL_DIST=1 but no shenyu-${kind} distribution tarball exists; run the Maven build first"
    copy_docker_context_files "${kind}" "${context_dir}"
    cp "${tarball}" "${context_dir}/target/"
    printf '%s\n' "${context_dir}"
}

build_image() {
    local kind="$1" context_dir app_name image candidate tarball_file=""
    context_dir=$(if [ "${USE_REAL_DIST}" = "1" ]; then build_real_context "${kind}"; else
        case "${kind}" in
            bootstrap) build_stub_context "bootstrap" "org.apache.shenyu.bootstrap" "ShenyuBootstrapApplication" ;;
            admin) build_stub_context "admin" "org.apache.shenyu.admin" "ShenyuAdminBootstrap" ;;
            *) fail "unknown distribution kind: ${kind}" ;;
        esac
    fi)
    [ -n "${context_dir}" ] && [ -d "${context_dir}" ] || fail "failed to prepare the ${kind} docker build context"
    for candidate in "${context_dir}/target/"*.tar.gz; do
        if [ -f "${candidate}" ]; then
            tarball_file="${candidate}"
            break
        fi
    done
    [ -n "${tarball_file}" ] || fail "no distribution tarball found in ${context_dir}/target"
    app_name=$(basename "${tarball_file}" .tar.gz)
    image="${SHENYU_SMOKE_IMAGE_PREFIX:-shenyu-smoke}-${kind}:test"
    log "building ${image} from ${app_name}"
    if ! docker build --quiet \
        --build-arg "APP_NAME=${app_name}" \
        --file "$(to_native_path "${context_dir}/docker/Dockerfile")" \
        --tag "${image}" \
        "$(to_native_path "${context_dir}")" >/dev/null; then
        fail "docker build failed for ${image}"
    fi
    printf '%s\n' "${image}"
}

image_size() {
    docker image inspect --format '{{.Size}}' "$1"
}

# Match against captured logs instead of piping them into grep: with
# `set -o pipefail`, `docker logs | grep -q` can fail with 141 (SIGPIPE) when the
# logs are larger than the pipe buffer and grep exits after the first match.
logs_contain() {
    local logs="$1" marker="$2"
    case "${logs}" in
        *"${marker}"*) return 0 ;;
        *) return 1 ;;
    esac
}

container_logs() {
    docker logs "$1" 2>&1 || true
}

assert_non_root() {
    local image="$1" uid
    uid=$(docker run --rm --entrypoint id "${image}" -u)
    [ "${uid}" = "${EXPECTED_UID}" ] || fail "${image} runs as uid ${uid}, expected ${EXPECTED_UID}"
    log "OK ${image} runs as uid ${uid}"
}

assert_read_only_rootfs() {
    local image="$1" result
    # Probe as root so the check depends on the read-only mount, not on the
    # non-root user being unable to write /opt in the writable case. The result
    # is read back explicitly, so a broken image (no shell, wrong arch) fails
    # here instead of being mistaken for a read-only filesystem.
    result=$(docker run --rm --read-only --user root --entrypoint sh "${image}" \
        -c 'if touch /read-only-probe 2>/dev/null; then echo writable; else echo readonly; fi' 2>/dev/null || true)
    [ "${result}" = "readonly" ] || fail "${image} root filesystem probe returned '${result}', expected 'readonly'"
    log "OK ${image} root filesystem is read-only"
}

assert_healthcheck_configured() {
    local image="$1" config
    config=$(docker image inspect --format '{{json .Config.Healthcheck}}' "${image}")
    logs_contain "${config}" "actuator/health" || fail "${image} does not declare a HEALTHCHECK on /actuator/health"
    logs_contain "${config}" "wget" || fail "${image} HEALTHCHECK does not use wget, which the image must ship"
    log "OK ${image} declares a HEALTHCHECK on /actuator/health"
}

wait_for_docker_healthy() {
    local container="$1" attempt status
    for attempt in $(seq 1 60); do
        status=$(docker inspect -f '{{if .State.Health}}{{.State.Health.Status}}{{else}}none{{end}}' "${container}" 2>/dev/null || echo unknown)
        case "${status}" in
            healthy) return 0 ;;
            unhealthy) fail "container ${container} reported unhealthy" ;;
        esac
        sleep 5
    done
    fail "container ${container} never became healthy (last status: ${status})"
}

wait_for_log() {
    local container="$1" marker="$2" attempt
    for attempt in $(seq 1 90); do
        if logs_contain "$(container_logs "${container}")" "${marker}"; then
            return 0
        fi
        if [ "$(docker inspect -f '{{.State.Running}}' "${container}" 2>/dev/null || echo false)" != "true" ]; then
            docker logs "${container}" >&2 || true
            fail "container ${container} stopped before logging ${marker}"
        fi
        sleep 1
    done
    docker logs "${container}" >&2 || true
    fail "timed out waiting for ${marker} in container ${container}"
}

jvm_args() {
    local kind="$1"
    if [ -n "${JVM_ARGS_OVERRIDE}" ]; then
        printf '%s' "${JVM_ARGS_OVERRIDE}"
    elif [ "${USE_REAL_DIST}" = "1" ] && [ "${kind}" = "admin" ]; then
        printf '%s' '-Xmx1g -Xms512m'
    else
        printf '%s' '-Xmx256m -Xms128m'
    fi
}

dist_port() {
    case "$1" in
        bootstrap) printf '9195' ;;
        admin) printf '9095' ;;
        *) fail "unknown distribution kind: $1" ;;
    esac
}

start_container() {
    local kind="$1" image="$2" conf_dir="$3" jvm_env="$4" extra_env="${5:-}" read_only="${6:-1}" name
    local env_args=(--env "${jvm_env}=$(jvm_args "${kind}")")
    if [ -n "${extra_env}" ]; then
        env_args+=(--env "${extra_env}")
    fi
    local tmp_args=()
    if [ "${read_only}" = "1" ]; then
        tmp_args=(--read-only --tmpfs /tmp:rw,size=64m)
    fi
    name="shenyu-smoke-$$-${RANDOM}"
    CONTAINERS+=("${name}")
    docker run --detach \
        --name "${name}" \
        "${tmp_args[@]}" \
        "${env_args[@]}" \
        --volume "$(to_native_path "${conf_dir}"):$(dist_mount_path "${kind}")/conf-ext:ro" \
        "${image}" >/dev/null
    printf '%s\n' "${name}"
}

stop_container_gracefully() {
    local name="$1" marker="$2" code waited=0
    docker kill --signal=SIGTERM "${name}" >/dev/null
    # Never block forever on a container that ignores SIGTERM.
    while [ "$(docker inspect -f '{{.State.Running}}' "${name}" 2>/dev/null || echo false)" = "true" ]; do
        waited=$((waited + 1))
        if [ "${waited}" -ge 60 ]; then
            docker kill --signal=SIGKILL "${name}" >/dev/null 2>&1 || true
            fail "container ${name} did not exit within 60s after SIGTERM"
        fi
        sleep 1
    done
    code=$(docker inspect -f '{{.State.ExitCode}}' "${name}")
    logs_contain "$(container_logs "${name}")" "${marker}" || fail "shutdown marker ${marker} was not logged by ${name}"
    # A JVM that handles SIGTERM runs its shutdown hooks and exits with 143
    # (128 + SIGTERM); 137 would mean it had to be SIGKILLed after the grace period.
    case "${code}" in
        0|143) ;;
        *) fail "container ${name} exited with ${code} after SIGTERM" ;;
    esac
    log "OK ${name} stopped gracefully with ${marker} (exit code ${code})"
    docker rm -f "${name}" >/dev/null 2>&1 || true
}

smoke_image() {
    local kind="$1" jvm_env="$2" image="$3" conf_dir name
    conf_dir="${WORK_DIR}/conf-ext-${kind}"
    mkdir -p "${conf_dir}"

    assert_non_root "${image}"
    assert_read_only_rootfs "${image}"
    assert_healthcheck_configured "${image}"
    log "${image} image size: $(image_size "${image}") bytes"

    if [ "${USE_REAL_DIST}" = "1" ]; then
        local extra_env="" started=${SECONDS}
        if [ "${kind}" = "admin" ]; then
            # Current admin releases require an explicit JWT signing key.
            extra_env="SHENYU_JWT_SECRETKEY=shenyu-smoke-jwt-secret-key"
        fi
        name=$(start_container "${kind}" "${image}" "${conf_dir}" "${jvm_env}" "${extra_env}")
        wait_for_log "${name}" "Started Shenyu"
        logs_contain "$(container_logs "${name}")" "environment variable ${jvm_env} set" \
            || fail "the ${jvm_env} override was not applied"
        # Validate the endpoint used by the image HEALTHCHECK and the Kubernetes probes.
        local health
        health=$(docker exec "${name}" wget -q -O - "http://127.0.0.1:$(dist_port "${kind}")/actuator/health")
        logs_contain "${health}" '"status":"UP"' \
            || fail "the actuator health endpoint of ${image} did not report UP: ${health}"
        log "OK ${image} reports status UP on /actuator/health after $((SECONDS - started))s"
        wait_for_docker_healthy "${name}"
        log "OK ${image} Docker HEALTHCHECK reports healthy after $((SECONDS - started))s"
        # Logback registers a JVM shutdown hook, so this line proves the SIGTERM
        # was delivered to the JVM and its shutdown hooks ran.
        stop_container_gracefully "${name}" "Logback context being closed via shutdown hook"
        return 0
    fi

    printf 'shenyu-smoke\n' >"${conf_dir}/stub.marker"
    name=$(start_container "${kind}" "${image}" "${conf_dir}" "${jvm_env}")
    wait_for_log "${name}" "READY"
    local logs
    logs=$(container_logs "${name}")
    logs_contain "${logs}" "TMP_OK" || fail "the JVM could not use a writable temp directory"
    logs_contain "${logs}" "CONF_OVERRIDE_OK:shenyu-smoke" \
        || fail "conf-ext overrides were not visible on a read-only root filesystem"
    logs_contain "${logs}" "environment variable ${jvm_env} set" \
        || fail "the ${jvm_env} override was not applied"
    log "OK ${image} started read-only with conf-ext overrides and ${jvm_env}"
    stop_container_gracefully "${name}" "SHUTDOWN_HOOK_OK"
}

smoke_writable_conf_ext() {
    local kind="$1" image="$2" jvm_env="$3" conf_dir name logs
    conf_dir="${WORK_DIR}/conf-ext-${kind}"
    # Without --read-only the entrypoint must keep the legacy behavior of copying
    # conf-ext over conf.
    name=$(start_container "${kind}" "${image}" "${conf_dir}" "${jvm_env}" "" "0")
    wait_for_log "${name}" "READY"
    logs=$(container_logs "${name}")
    logs_contain "${logs}" "Applied conf-ext overrides" \
        || fail "the writable conf-ext copy path was not used by ${image}"
    logs_contain "${logs}" "CONF_OVERRIDE_OK:shenyu-smoke" \
        || fail "conf-ext overrides were not copied into conf for ${image}"
    stop_container_gracefully "${name}" "SHUTDOWN_HOOK_OK"
    log "OK writable conf-ext copy path still applies overrides"
}

smoke_restart_same_config() {
    local kind="$1" image="$2" jvm_env="$3" conf_dir name
    conf_dir="${WORK_DIR}/conf-ext-${kind}"
    # Restarting the image with a different tag and the same mounted config shows
    # that overrides are not tied to the image content (a version rollback reuses
    # the same ConfigMap/Secret mounts).
    docker tag "${image}" "${image}-previous"
    name=$(start_container "${kind}" "${image}-previous" "${conf_dir}" "${jvm_env}")
    wait_for_log "${name}" "READY"
    logs_contain "$(container_logs "${name}")" "CONF_OVERRIDE_OK:shenyu-smoke" \
        || fail "the restarted image did not see the mounted conf-ext configuration"
    stop_container_gracefully "${name}" "SHUTDOWN_HOOK_OK"
    docker rmi -f "${image}-previous" >/dev/null 2>&1 || true
    log "OK restarting the image with the same mounted configuration keeps the overrides"
}

previous_jvm_env() {
    case "$1" in
        bootstrap) printf 'BOOT_JVM=-Xmx512m -Xms256m' ;;
        admin) printf 'ADMIN_JVM=-Xmx1g -Xms512m' ;;
        *) fail "unknown distribution kind: $1" ;;
    esac
}

start_previous_container() {
    local kind="$1" image="$2" conf_dir="$3" name extra_env=()
    name="shenyu-smoke-previous-$$-${RANDOM}"
    CONTAINERS+=("${name}")
    if [ "${kind}" = "admin" ]; then
        # Current admin releases fail fast without an explicit JWT signing key;
        # pass one so a previous release can start too (it is ignored if unused).
        extra_env=(--env "SHENYU_JWT_SECRETKEY=shenyu-smoke-jwt-secret-key")
    fi
    # Previous releases run as root/shenyu with a writable root filesystem and
    # copy conf-ext over conf, so they are started with their own contract. The
    # point of the check is that the same ConfigMap/conf-ext mounts and the same
    # environment still let the previous image start.
    docker run --detach \
        --name "${name}" \
        "${extra_env[@]}" \
        --env "$(previous_jvm_env "${kind}")" \
        --volume "$(to_native_path "${conf_dir}"):$(dist_mount_path "${kind}")/conf-ext:ro" \
        "${image}" >/dev/null
    printf '%s\n' "${name}"
}

stop_previous_container() {
    local name="$1" code
    docker stop --time 60 "${name}" >/dev/null
    code=$(docker inspect -f '{{.State.ExitCode}}' "${name}")
    case "${code}" in
        0|143) ;;
        *) fail "previous-release container ${name} exited with ${code} after SIGTERM" ;;
    esac
    docker rm -f "${name}" >/dev/null 2>&1 || true
}

smoke_previous_tag() {
    local kind="$1" image conf_dir name
    image="${PREVIOUS_IMAGE_PREFIX}-${kind}:${PREVIOUS_TAG}"
    conf_dir="${WORK_DIR}/conf-ext-${kind}"
    mkdir -p "${conf_dir}"
    log "pulling ${image} for the cross-version rollback check"
    docker pull "${image}" >/dev/null || fail "could not pull the previous release image ${image}"
    name=$(start_previous_container "${kind}" "${image}" "${conf_dir}")
    wait_for_log "${name}" "Started Shenyu"
    stop_previous_container "${name}"
    log "OK previous release ${image} starts and stops with the same mounted configuration"
}

assert_dockerfile_base() {
    local kind
    for kind in bootstrap admin; do
        grep -q "^FROM ${BASE_IMAGE}" "$(repo_dist_dir "${kind}")/docker/Dockerfile" \
            || fail "the ${kind} Dockerfile must use FROM ${BASE_IMAGE}; bump the Dockerfiles, this script and docs/container-runtime.md together"
        grep -q "^FROM ${PREPARE_IMAGE} AS prepare" "$(repo_dist_dir "${kind}")/docker/Dockerfile" \
            || fail "the ${kind} Dockerfile must unpack the distribution on FROM ${PREPARE_IMAGE} AS prepare"
    done
    log "OK both Dockerfiles pin the expected base image ${BASE_IMAGE} (prepare: ${PREPARE_IMAGE})"
}

main() {
    require_tools
    [ "${EXPECTED_UID}" != "0" ] || fail "the expected UID must not be 0: the image contract requires a non-root user"
    log "base image: ${BASE_IMAGE}"
    assert_dockerfile_base
    local bootstrap_image admin_image
    bootstrap_image=$(build_image bootstrap)
    admin_image=$(build_image admin)

    smoke_image "bootstrap" "BOOT_JVM" "${bootstrap_image}"
    smoke_image "admin" "ADMIN_JVM" "${admin_image}"

    if [ "${USE_REAL_DIST}" = "0" ]; then
        smoke_writable_conf_ext "bootstrap" "${bootstrap_image}" "BOOT_JVM"
        smoke_restart_same_config "bootstrap" "${bootstrap_image}" "BOOT_JVM"
    fi

    if [ -n "${PREVIOUS_TAG}" ]; then
        log "cross-version rollback check against ${PREVIOUS_IMAGE_PREFIX}-*:${PREVIOUS_TAG}"
        smoke_previous_tag bootstrap
        smoke_previous_tag admin
    fi

    log "all container runtime smoke checks passed"
}

main "$@"
