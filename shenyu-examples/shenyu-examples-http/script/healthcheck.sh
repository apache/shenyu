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

sleep 5

status=`curl -s -o /dev/null -w "%{http_code}" -X POST -H "Content-Type:application/json" http://localhost:31195/http/order/save --data '{"name":"test", "id": 123}'`

sleep 3

if [ $status -eq 200 ]; then
    echo -e "Success to send request: $status"
    echo -e "\n-------------------"
    exit 0
fi
echo -e "Failed to send request from shenyu-bootstrap to http example: $status"
echo -e "\n-------------------"
exit 1
