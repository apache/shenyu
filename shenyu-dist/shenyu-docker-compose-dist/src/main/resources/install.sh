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

function check_sed() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        if ! which gsed >/dev/null 2>&1; then
            echo "Please install gsed, you can run 'brew install gnu-sed'"
            exit 1
        fi
    elif [[ "$OSTYPE" == "linux"* ]]; then
        if ! which sed >/dev/null 2>&1; then
            echo "Please install sed, you can run 'apt-get install sed'"
            exit 1
        fi
    else
        echo "Unknown operating system: $OSTYPE"
        exit 1
    fi
}

version=${1}

if [[ $version == '' || $version == 'latest' ]]; then
  echo "The version will be set to latest."
  version='master'
fi

if [[ $version != v* && $version != 'master' ]]; then
  echo "The version should start with 'v', such as 'v2.4.2' or 'latest'."
  exit 1
fi

echo "current version: ${version}"

mkdir shenyu-${version}

cd shenyu-${version}

mkdir -p {shenyu-bootstrap,shenyu-admin}/{conf,logs}

echo "Downloading docker-compose configuration ..."
curl -sSl https://raw.githubusercontent.com/apache/shenyu/master/shenyu-dist/shenyu-docker-compose-dist/src/main/resources/docker-compose.yaml > docker-compose.yaml

if [ "$version" != "master" ];then
    check_sed
    newVersion=${version#"v"}
    if [[ "$OSTYPE" == "darwin"* ]]; then
        gsed -i 's/latest/'"${newVersion}"'/g' docker-compose.yaml
    elif [[ "$OSTYPE" == "linux"* ]]; then
        sed -i 's/latest/'"${newVersion}"'/g' docker-compose.yaml
    fi
fi

mkdir -p shenyu-admin/ext-lib
echo "Downloading mysql-connector.jar ..."
(cd shenyu-admin/ext-lib && curl -o mysql-connector.jar https://repo1.maven.org/maven2/mysql/mysql-connector-java/8.0.18/mysql-connector-java-8.0.18.jar)

printf '\n'
echo "Downloading shenyu-admin configuration ..."
(cd shenyu-admin/conf/ && curl -OOOO https://raw.githubusercontent.com/apache/shenyu/${version}/shenyu-admin/src/main/resources/{application-mysql.yml,application.yml,application-h2.yml,application-pg.yml})
(cd shenyu-admin/conf/ && curl -O https://raw.githubusercontent.com/apache/shenyu/${version}/shenyu-dist/shenyu-admin-dist/src/main/resources/logback.xml)

printf '\n'
echo "Downloading shenyu-bootstrap configuration ..."
(cd shenyu-bootstrap/conf/ && curl -O https://raw.githubusercontent.com/apache/shenyu/${version}/shenyu-bootstrap/src/main/resources/application.yml)
(cd shenyu-bootstrap/conf/ && curl -O https://raw.githubusercontent.com/apache/shenyu/${version}/shenyu-dist/shenyu-bootstrap-dist/src/main/resources/logback.xml)

printf '\n'
echo "Next steps? Please modify the configuration in ./shenyu-${version}"
echo "And then, you can run docker-compose"
echo "For more detail, see https://shenyu.apache.org/docs/index"
