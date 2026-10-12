#!/bin/bash
set -e
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

kind load docker-image "shenyu-examples-http:latest"
kind load docker-image "apache/shenyu-integrated-test-k8s-gateway-api-http:latest"

# Install Gateway API CRD before creating Gateway/HTTPRoute resources
kubectl apply -f https://github.com/kubernetes-sigs/gateway-api/releases/download/v1.5.1/standard-install.yaml

kubectl apply -f ./shenyu-examples/shenyu-examples-http/k8s/shenyu-examples-http.yml
kubectl apply -f ./shenyu-integrated-test/shenyu-integrated-test-k8s-gateway-api-http/deploy/deploy-shenyu.yaml
kubectl apply -f ./shenyu-examples/shenyu-examples-http/k8s/gateway-api.yml
