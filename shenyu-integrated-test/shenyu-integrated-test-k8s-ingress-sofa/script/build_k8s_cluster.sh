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

kind load docker-image "shenyu-examples-sofa:latest"
kind load docker-image "apache/shenyu-integrated-test-k8s-ingress-sofa:latest"
kubectl apply -f ./shenyu-examples/shenyu-examples-sofa/shenyu-examples-sofa-service/k8s/shenyu-zookeeper.yml
kubectl wait --for=condition=Ready pod -l app=shenyu-zk -n shenyu-ingress
kubectl apply -f ./shenyu-examples/shenyu-examples-sofa/shenyu-examples-sofa-service/k8s/shenyu-examples-sofa.yml
kubectl wait --for=condition=Ready pod -l app=shenyu-examples-sofa -n shenyu-ingress
kubectl apply -f ./shenyu-integrated-test/shenyu-integrated-test-k8s-ingress-sofa/deploy/deploy-shenyu.yaml
kubectl wait --for=condition=Ready pod -l app=shenyu-ingress-controller -n shenyu-ingress
kubectl apply -f ./shenyu-examples/shenyu-examples-sofa/shenyu-examples-sofa-service/k8s/ingress.yml

kubectl get pod -o wide -n shenyu-ingress

echo "Waiting for shenyu-examples-sofa-service to be ready"
kubectl logs "$(kubectl get pod -o wide -n shenyu-ingress| grep shenyu-examples-sofa-deployment | awk '{print $1}')" -n shenyu-ingress