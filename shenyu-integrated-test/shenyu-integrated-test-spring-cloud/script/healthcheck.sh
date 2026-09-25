#!/bin/bash
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
#

PRGDIR=`dirname "$0"`
# Waiting for service registration
sleep 60s
for service in `grep -v -E "^$|^#" "${PRGDIR}/services.list"`
do
    ready=0
    for loop in $(seq 1 "${MAX_RETRIES:-30}")
    do
        status=$(curl --connect-timeout 5 --max-time 10 -o /dev/null -s -w "%{http_code}" "$service") || status=000
        echo -e "curl $service response $status"
        if [ "$status" = "200" ]; then
            ready=1
            break
        fi

        sleep 2
    done
    if [ "$ready" -ne 1 ]; then
        echo "Service $service failed healthcheck after ${MAX_RETRIES:-30} attempts" >&2
        exit 1
    fi
done
sleep 30s

registered=0
for loop in $(seq 1 "${MAX_RETRIES:-30}")
do
  app_count=$(wget --timeout=10 --tries=1 -q -O- http://shenyu-examples-eureka:8761/eureka/apps | grep "<application>" | wc -l | xargs)
  echo "app count ${app_count}"
  if [ $app_count -gt 1  ]; then
      registered=1
      break
  fi
  sleep 2
done

if [ "$registered" -ne 1 ]; then
    echo "Eureka applications did not register before the retry limit" >&2
    exit 1
fi

curl -s -XGET http://shenyu-examples-eureka:8761/eureka/apps > eureka.log
cat eureka.log

echo -e "\n-------------------"
