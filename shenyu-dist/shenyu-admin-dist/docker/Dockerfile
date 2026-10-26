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

# The prepare stage only unpacks the distribution tarball. Keep it on the same
# maintained Alpine generation as the runtime image so no EOL base is pulled.
FROM alpine:3.24 AS prepare

ARG APP_NAME

ENV LOCAL_PATH /opt/shenyu-admin

ADD target/${APP_NAME}.tar.gz /opt
RUN mv /opt/${APP_NAME} ${LOCAL_PATH}

# Runtime: a maintained Corretto 17 JDK on Alpine (linux/amd64 and linux/arm64).
# Keep the exact patch tag in sync with shenyu-bootstrap-dist; bump both together
# and record the change in docs/container-runtime.md.
FROM amazoncorretto:17.0.21-alpine3.24

# Stable non-root UID/GID: Kubernetes securityContext.runAsUser can rely on it and
# file ownership survives image rebuilds. wget is used by HEALTHCHECK; curl is kept
# for operators that already use it from inside the container.
RUN apk --no-cache add wget curl \
    && addgroup -S -g 10001 shenyu \
    && adduser -S -u 10001 -G shenyu -h /home/shenyu shenyu \
    && mkdir -p /home/shenyu \
    && chown -R shenyu:shenyu /home/shenyu

ENV LOCAL_PATH /opt/shenyu-admin
ENV ADMIN_JVM ""
# The JVM only needs a writable temp directory at runtime. Mount an emptyDir or
# tmpfs on /tmp when the root filesystem is read-only (see docs/container-runtime.md).
ENV TMPDIR /tmp

COPY --chown=shenyu:shenyu --from=prepare ${LOCAL_PATH} ${LOCAL_PATH}
COPY --chown=shenyu:shenyu docker/logback.xml ${LOCAL_PATH}/conf/logback.xml
COPY --chown=shenyu:shenyu docker/entrypoint.sh ${LOCAL_PATH}/entrypoint.sh
RUN chmod 0755 ${LOCAL_PATH}/entrypoint.sh

WORKDIR ${LOCAL_PATH}

# Numeric user on purpose: a named user prevents kubelet from verifying a pod
# that only sets securityContext.runAsNonRoot: true (restricted PodSecurity).
USER 10001:10001

EXPOSE 9095

# The entrypoint execs the JVM, so the JVM stays PID 1 and receives SIGTERM
# directly. Spring Boot then runs its shutdown hooks before the process exits.
STOPSIGNAL SIGTERM

HEALTHCHECK --interval=30s --timeout=5s --start-period=90s --retries=3 \
    CMD wget -q -O /dev/null http://127.0.0.1:9095/actuator/health || exit 1

ENTRYPOINT ["/bin/sh", "entrypoint.sh"]
