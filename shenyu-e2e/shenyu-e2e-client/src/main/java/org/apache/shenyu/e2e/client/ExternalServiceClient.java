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

import org.apache.shenyu.e2e.annotation.ExternalService;

import jakarta.annotation.Nonnull;
import java.util.Properties;

/**
 * External service client.
 */
@ExternalService
public class ExternalServiceClient extends BaseClient {
    
    private final String scenarioId;
    
    private final String serviceName;
    
    private final String baseUrl;
    
    private final Properties properties;
    
    /**
     * Instantiates a new External service client.
     *
     * @param scenarioId the scenario id
     * @param serviceName the service name
     * @param baseUrl the base url
     * @param properties the properties
     */
    public ExternalServiceClient(final String scenarioId, @Nonnull final String serviceName,
                                 final String baseUrl, final Properties properties) {
        super(serviceName);
        this.scenarioId = scenarioId;
        this.serviceName = serviceName;
        this.baseUrl = baseUrl;
        this.properties = properties;
    }
}
