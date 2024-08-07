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
for service in `grep -v -E "^$|^#" ${PRGDIR}/services.list`
do
    for loop in `seq 1 30`
    do
        status=`curl -o /dev/null -s -w %{http_code} $service`
        echo -e "curl $service response $status"
        if [ $status -eq 200  ]; then
            break
        fi

        sleep 2
    done
done
sleep 30s

for loop in `seq 1 30`
do
  app_count=$(wget -q -O- http://shenyu-examples-eureka:8761/eureka/apps | grep "<application>" | wc -l | xargs)
  echo "app count ${app_count}"
  if [ $app_count -gt 1  ]; then
      break
  fi
  sleep 2
done

curl -s -XGET http://shenyu-examples-eureka:8761/eureka/apps > eureka.log
cat eureka.log

echo -e "\n-------------------"
