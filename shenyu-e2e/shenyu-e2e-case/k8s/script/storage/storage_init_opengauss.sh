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

mkdir -p /tmp/shenyu-e2e/opengauss/schema
mkdir -p /tmp/shenyu-e2e/opengauss/driver

wget -O /tmp/shenyu-e2e/opengauss/driver/opengauss-jdbc-5.1.0-og.jar \
  https://repo1.maven.org/maven2/org/opengauss/opengauss-jdbc/5.1.0-og/opengauss-jdbc-5.1.0-og.jar --no-check-certificate || \
  wget -O /tmp/shenyu-e2e/opengauss/driver/opengauss-jdbc-5.1.0-og.jar \
  https://repo1.maven.org/maven2/org/opengauss/opengauss-jdbc/5.1.0-og/opengauss-jdbc-5.1.0-og.jar --no-check-certificate

cp db/init/og/create-table.sql /tmp/shenyu-e2e/opengauss/schema/create-table.sql