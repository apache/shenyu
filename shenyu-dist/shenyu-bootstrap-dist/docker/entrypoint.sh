#!/bin/bash

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

[[ -d ./conf-ext ]] && cp -f ./conf-ext/* ./conf

DEPLOY_DIR=$(pwd)
EXT_LIB=${DEPLOY_DIR}/ext-lib

CLASS_PATH=.:${DEPLOY_DIR}/conf:${DEPLOY_DIR}/lib/*:${EXT_LIB}/*
if [ -z "${BOOT_JVM}" ]; then
    JAVA_OPTS=" -server -Xmx2g -Xms2g -Xmn1g -Xss512k -XX:+DisableExplicitGC   -XX:LargePageSizeInBytes=128m"
    JAVA_OPTS="${JAVA_OPTS} -XX:+UseFastAccessorMethods  -XX:+UseConcMarkSweepGC -XX:+CMSParallelRemarkEnabled -XX:+UseCMSInitiatingOccupancyOnly  -XX:CMSInitiatingOccupancyFraction=70"
    echo "Use default jvm options: $JAVA_OPTS"
else
    JAVA_OPTS=${BOOT_JVM}
    echo "Start with the environment variable JAVA_OPTS set: $JAVA_OPTS"
fi

echo "Starting the Apache ShenYu Bootstrap ..."
exec ${JAVA_HOME}/bin/java ${JAVA_OPTS} -classpath ${CLASS_PATH} org.apache.shenyu.bootstrap.ShenyuBootstrapApplication "$@"
