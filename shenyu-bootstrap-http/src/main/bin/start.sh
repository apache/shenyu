#!/bin/sh
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements. See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License. You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -eu
APP_HOME=$(CDPATH='' cd -- "$(dirname -- "$0")/.." && pwd)
JAVA=java
if [ -n "${JAVA_HOME:-}" ]; then JAVA="$JAVA_HOME/bin/java"; fi
cd "$APP_HOME"
# JAVA_OPTS intentionally supports multiple JVM arguments, as in the full distribution.
exec "$JAVA" ${JAVA_OPTS:-} -cp "$APP_HOME/conf:$APP_HOME/lib/*:$APP_HOME/ext-lib/*" \
  org.apache.shenyu.bootstrap.http.ShenyuHttpApplication "$@"
