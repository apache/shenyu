#!/bin/sh

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

# POSIX sh on purpose: both distribution images are Alpine based and do not ship
# bash. Keep this script free of bash-only syntax ([[ ]], arrays, source, ...).

set -eu

umask 027

DEPLOY_DIR=$(pwd)
EXT_LIB=${DEPLOY_DIR}/ext-lib
EXT_CONF=""

# Optional configuration overrides. The legacy behavior copies ./conf-ext into
# ./conf; when the image filesystem is read-only, prepend ./conf-ext to the
# classpath instead so the overrides still win without writing to the image.
if [ -d ./conf-ext ]; then
    if ( : > ./conf/.shenyu-conf-probe ) 2>/dev/null; then
        rm -f ./conf/.shenyu-conf-probe
        if cp -f ./conf-ext/* ./conf 2>/dev/null; then
            echo "Applied conf-ext overrides into ${DEPLOY_DIR}/conf"
        elif [ -n "$(ls -A ./conf-ext 2>/dev/null)" ]; then
            # Partial copy: fall back to the classpath so every override still wins.
            EXT_CONF=./conf-ext
            echo "WARN: conf-ext entries could not be fully copied into ${DEPLOY_DIR}/conf; loading ./conf-ext from the classpath" >&2
        fi
    else
        EXT_CONF=./conf-ext
        echo "Read-only conf directory detected; loading ./conf-ext from the classpath"
    fi
fi

CLASS_PATH=.
if [ -n "${EXT_CONF}" ]; then
    CLASS_PATH="${CLASS_PATH}:${EXT_CONF}"
fi
CLASS_PATH="${CLASS_PATH}:${DEPLOY_DIR}/conf:${DEPLOY_DIR}/lib/*:${EXT_LIB}/*"

if [ -z "${ADMIN_JVM:-}" ]; then
    JAVA_OPTS=" -server -Xmx2g -Xms2g -Xmn1g -Xss328k -XX:+DisableExplicitGC  -XX:LargePageSizeInBytes=128m"
    echo "Use default jvm options: ${JAVA_OPTS}"
else
    JAVA_OPTS=${ADMIN_JVM}
    echo "Start with the environment variable ADMIN_JVM set: ${JAVA_OPTS}"
fi

if [ -n "${JAVA_HOME:-}" ]; then
    JAVA_BIN=${JAVA_HOME}/bin/java
else
    JAVA_BIN=java
fi

echo "Starting the Apache ShenYu Admin ..."

# exec keeps the JVM as PID 1 so SIGTERM from the container runtime reaches the
# Spring Boot shutdown hooks directly and the JVM exit code is propagated.
# shellcheck disable=SC2086 # JAVA_OPTS is an option string and must word-split.
exec "${JAVA_BIN}" -Djava.io.tmpdir="${TMPDIR:-/tmp}" ${JAVA_OPTS} -classpath "${CLASS_PATH}" org.apache.shenyu.admin.ShenyuAdminBootstrap "$@"
