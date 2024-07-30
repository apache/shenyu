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

kind load docker-image "shenyu-examples-eureka:latest"
kind load docker-image "shenyu-examples-springcloud:latest"
kind load docker-image "apache/shenyu-integrated-test-k8s-ingress-spring-cloud:latest"
kubectl apply -f ./shenyu-integrated-test/shenyu-integrated-test-k8s-ingress-spring-cloud/deploy/deploy-shenyu.yaml
kubectl apply -f ./shenyu-examples/shenyu-examples-eureka/k8s/shenyu-examples-eureka.yml
kubectl wait --for=condition=Ready pod -l app=shenyu-examples-eureka-deployment -n shenyu-ingress
kubectl apply -f ./shenyu-examples/shenyu-examples-springcloud/k8s/shenyu-examples-springcloud.yml
kubectl wait --for=condition=Ready pod -l app=shenyu-examples-springcloud-deployment -n shenyu-ingress
kubectl apply -f ./shenyu-examples/shenyu-examples-springcloud/k8s/ingress.yml
for loop in `seq 1 30`
do
  curl -s -X GET http://localhost:30761/eureka/apps
  sleep 2
done