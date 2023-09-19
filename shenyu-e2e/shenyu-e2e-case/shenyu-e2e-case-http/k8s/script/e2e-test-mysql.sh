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

## init kubernetes for mysql
curPath=$(readlink -f "$(dirname "$0")")
PRGDIR=`dirname "$curPath"`
echo $PRGDIR
kubectl apply -f ${PRGDIR}/shenyu-mysql.yml
sleep 10s

kubectl get pod -o wide

sleep 60s

kubectl get pod -o wide

echo -e `nc -z -v -w30 shenyu-mysql 30306`

kubectl apply -f ${PRGDIR}/shenyu-deployment-mysql.yml
kubectl apply -f ${PRGDIR}/shenyu-app-service-mysql.yml

kubectl get pod -o wide

sleep 60s

kubectl run -it --rm --image=mysql:8.0 --restart=Never mysql-client -- mysql -h shenyu-mysql -uroot -pshenyue2e -e "show databases;"
kubectl get pod -o wide

chmod +x ${curPath}/healthcheck.sh
sh ${curPath}/healthcheck.sh mysql
#
#kubectl logs -l app=shenyu-admin-mysql
#
#sleep 10s
#
#kubectl -n kube-system  get pods | grep Evicted |awk '{print$1}'|xargs kubectl -n kube-system delete pods
#kubectl get pod -o wide
#
## execute healthcheck.sh
#chmod +x ${curPath}/healthcheck.sh
#sh ${curPath}/healthcheck.sh mysql
#
### run e2e-test
#curl http://localhost:31196/actuator/shenyu/pluginData

### wait shenyu-admin and shenyu-bootstrap start

# get shenyu-admin pod name
#
#shenyu_admin_pod_name=`kubectl get pod -n kube-system | grep shenyu-admin | awk '{print $1}'`
#
## get shenyu-bootstrap pod name
#
#shenyu_bootstrap_pod_name=`kubectl get pod -n kube-system | grep shenyu-bootstrap | awk '{print $1}'`
#
## get shenyu-admin pod status
#
#shenyu_admin_pod_status=`kubectl get pod -n kube-system | grep shenyu-admin | awk '{print $3}'`
