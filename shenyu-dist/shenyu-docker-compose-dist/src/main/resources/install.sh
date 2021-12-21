#!/bin/bash

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

version=${1}
storage=${2}

mkdir shenyu-${version}

cd shenyu-${version}

mkdir -p {shenyu-bootstrap,shenyu-admin}/{conf,logs}

curl -sSl https://raw.githubusercontent.com/apache/incubator-shenyu/${version}/shenyu-dist/shenyu-docker-compose-dist/src/main/resources/stand-alone-${storage}/docker-compose.yaml > docker-compose.yaml

# shellcheck disable=SC2205
if ( "${storage}" = 'mysql') then
  mkdir -p shenyu-admin/ext-lib
  (cd shenyu-admin/ext-lib && curl -sSl https://repo1.maven.org/maven2/mysql/mysql-connector-java/8.0.18/mysql-connector-java-8.0.18.jar > mysql-connector.jar)
fi

(cd shenyu-admin/conf/ && curl -OOO https://raw.githubusercontent.com/apache/incubator-shenyu/${version}/shenyu-admin/src/main/resources/{application-mysql.yml,logback.xml,application.yml})
(cd shenyu-bootstrap/conf/ && curl -OOO https://raw.githubusercontent.com/apache/incubator-shenyu/${version}/shenyu-bootstrap/src/main/resources/{application-local.yml,logback.xml,application.yml})

docker-compose up -d