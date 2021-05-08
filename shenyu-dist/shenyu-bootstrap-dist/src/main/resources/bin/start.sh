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

if [[ -n "$JAVA_HOME" ]] && [[ -x "$JAVA_HOME/bin/java" ]];  then
    JAVA="$JAVA_HOME/bin/java"
elif type -p java; then
    JAVA=java
else
    echo "Error: JAVA_HOME environment variable is not set." 1>&2
    exit 1
fi

PRG_DIR=$(dirname "$0")

APP="$PRG_DIR/../lib/shenyu-bootstrap.jar"
BASE_DIR="$PRG_DIR/.."

[[ -z "$LOG_DIR" ]] && LOG_DIR="$BASE_DIR/logs"
[[ -d "$LOG_DIR" ]] || mkdir -p "$LOG_DIR"

PID_FILE="$LOG_DIR/shenyu-bootstrap.pid"

JAVA_MEM_OPTS="-Xmx2048m -Xms2048m -Xmn1024m -Xss512k"

JAVA_GC_TUNE_OPTS="
  -XX:+UseParNewGC \
  -XX:+UseConcMarkSweepGC \
  -XX:+CMSParallelRemarkEnabled \
  -XX:+UseCMSInitiatingOccupancyOnly \
  -XX:MaxGCPauseMillis=850 \
  -XX:ParallelGCThreads=20 \
  -XX:CMSInitiatingOccupancyFraction=75 \
  -XX:+AggressiveOpts \
  -XX:+UseBiasedLocking \
  -XX:+DisableExplicitGC \
  -XX:+UseFastAccessorMethods \
  -XX:+HeapDumpOnOutOfMemoryError \
  "
#  -XX:+PrintGCDetail
#  -XX:+UseCMSCompactAtFullCollection

JAVA_GC_LOG_OPTS="-Xloggc:$LOG_DIR/gc.log"

JAVA_OPTS="$JAVA_OPTS $JAVA_MEM_OPTS $JAVA_GC_TUNE_OPTS $JAVA_GC_LOG_OPTS"

$JAVA -jar "$APP" --spring.config.location=file:"$BASE_DIR"/conf/ "$JAVA_OPTS" >> "${LOG_DIR}"/shenyu-bootstrap.out &

if [ $? -gt 0 ]; then
    echo "Starting $APP ERROR"
fi
    echo "Starting $APP OK"
echo $! > "$PID_FILE"
