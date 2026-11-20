#!/usr/bin/env bash
#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -euo pipefail

wrapper_jar=.mvn/wrapper/maven-wrapper.jar
wrapper_main=org/apache/maven/wrapper/MavenWrapperMain.class
if [[ -r "$wrapper_jar" ]] && jar tf "$wrapper_jar" | grep -Fx "$wrapper_main" > /dev/null; then
  exit 0
fi

wrapper_url="$(awk -F= '$1 == "wrapperUrl" { print substr($0, index($0, "=") + 1); exit }' .mvn/wrapper/maven-wrapper.properties)"
if [[ -z "$wrapper_url" ]]; then
  echo "Could not find wrapperUrl in .mvn/wrapper/maven-wrapper.properties."
  exit 1
fi

wrapper_tmp="$(mktemp .mvn/wrapper/maven-wrapper.jar.XXXXXX)"
trap 'rm -f "$wrapper_tmp"' EXIT
curl --fail --location --retry 5 --retry-all-errors --retry-delay 2 \
  --output "$wrapper_tmp" "$wrapper_url"
if ! jar tf "$wrapper_tmp" | grep -Fx "$wrapper_main" > /dev/null; then
  echo "Downloaded Maven wrapper JAR does not contain $wrapper_main."
  exit 1
fi
mv "$wrapper_tmp" "$wrapper_jar"
