/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.k8s.parser;

import io.kubernetes.client.openapi.apis.CoreV1Api;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;

import java.util.List;

/**
 * Parser of Kubernetes resource list.
 * Such as ingress, gateway, or even custom api resource.
 *
 * @param <T> resource type
 */
public interface K8sResourceListParser<T> {

    /**
     * Parse resource list to ShenyuMemoryConfig.
     *
     * @param resource resource
     * @param coreV1Api coreV1Api
     * @return ShenyuMemoryConfig
     */
    List<ShenyuMemoryConfig> parse(T resource, CoreV1Api coreV1Api);
}
