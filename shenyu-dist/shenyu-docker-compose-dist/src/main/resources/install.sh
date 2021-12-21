#!/bin/bash

version=${1}
storage=${2}

mkdir shenyu

cd shenyu

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