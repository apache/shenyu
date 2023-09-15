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

## init kubernetes for h2
pwd
PRGDIR=`dirname "./shenyu-e2e/shenyu-e2e-case/shenyu-e2e-case-http/k8s/shenyu-app-service-h2.yml"`
kubectl apply -f ${PRGDIR}/shenyu-deployment-h2.yml
kubectl apply -f ${PRGDIR}/shenyu-app-service-h2.yml

## check kubernetes for h2
#kubectl get pod -n kube-system | grep shenyu-admin
#kubectl get pod -n kube-system | grep shenyu-bootstrap

sleep 10s

# execute healthcheck.sh
chmod +x ${PRGDIR}/script/healthcheck.sh
bash ${PRGDIR}/script/healthcheck.sh h2

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
