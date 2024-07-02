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

package org.apache.shenyu.e2e.client;

import com.google.common.collect.Maps;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.e2e.annotation.ShenYuEnvironmentClient;

import jakarta.annotation.Nonnull;
import java.util.Map;
import java.util.Objects;

@ShenYuEnvironmentClient
public class EnvironmentClient {
    
    private final Map<String, BaseClient> environments = Maps.newConcurrentMap();
    
    /**
     * add client.
     *
     * @param baseClient client
     */
    public void add(final BaseClient baseClient) {
        if (Objects.isNull(baseClient)) {
            throw new IllegalArgumentException("baseClient is null");
        }
        if (StringUtils.isBlank(baseClient.getServiceName())) {
            throw new IllegalArgumentException("baseClient service name is blank");
        }
        environments.put(baseClient.getServiceName(), baseClient);
    }
    
    /**
     * get client.
     *
     * @param serviceName serviceName
     * @return service client
     */
    public BaseClient getClient(@Nonnull final String serviceName) {
        if (MapUtils.isEmpty(environments)) {
            throw new IllegalArgumentException("environments is empty");
        }
        BaseClient client = environments.get(serviceName);
        if (Objects.isNull(client)) {
            throw new IllegalArgumentException("service name not found: " + serviceName);
        }
        return client;
    }
}
