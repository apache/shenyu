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

package org.apache.shenyu.e2e.engine;

import com.google.common.collect.Maps;
import junit.framework.AssertionFailedError;
import org.apache.shenyu.e2e.client.ExternalServiceClient;
import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.common.IdGenerator;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure;
import org.apache.shenyu.e2e.engine.service.DockerServiceCompose;
import org.apache.shenyu.e2e.engine.service.HostServiceCompose;
import org.apache.shenyu.e2e.engine.service.ServiceCompose;

import java.util.Map;
import java.util.Objects;

import static org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.Mode.DOCKER;
import static org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.Mode.HOST;

/**
 * Start the project using the corresponding method based on the selected configuration.
 */
public class ShenYuExtensionContext {

    private final String scenarioId;
    
    private final ServiceCompose serviceCompose;

    private AdminClient adminClient;

    private GatewayClient gatewayClient;
    
    private Map<String, ExternalServiceClient> externalServiceClientMap;
    
    ShenYuExtensionContext(final ShenYuEngineConfigure config) {
        if (config.getMode() == HOST) {
            serviceCompose = new HostServiceCompose(config.getHostConfigure());
        } else if (config.getMode() == DOCKER) {
            serviceCompose = new DockerServiceCompose(config.getDockerConfigure());
        } else {
            throw new AssertionFailedError("Mode [" + config.getMode() + "] is not supported yet");
        }
        this.scenarioId = IdGenerator.generateScenarioId();
    }
    
    /**
     * start.
     */
    public void setup() {
        serviceCompose.start();
    }
    
    /**
     * get admin client.
     * @return AdminClient
     */
    public AdminClient getAdminClient() {
        if (Objects.isNull(adminClient)) {
            adminClient = serviceCompose.newAdminClient(scenarioId);
        }
        return adminClient;
    }
    
    /**
     * get gateway client.
     * @return GatewayClient
     */
    public GatewayClient getGatewayClient() {
        if (Objects.isNull(gatewayClient)) {
            gatewayClient = serviceCompose.newGatewayClient(scenarioId);
        }
        return gatewayClient;
    }
    
    /**
     * get external service client.
     * @param externalServiceName externalServiceName
     * @return ExternalServiceClient
     */
    public ExternalServiceClient getExternalServiceClient(final String externalServiceName) {
        if (Objects.isNull(externalServiceClientMap)) {
            externalServiceClientMap = Maps.newHashMap();
        }
        if (externalServiceClientMap.containsKey(externalServiceName)) {
            return externalServiceClientMap.get(externalServiceName);
        }
        ExternalServiceClient client = serviceCompose.newExternalServiceClient(externalServiceName);
        externalServiceClientMap.put(externalServiceName, client);
        return client;
    }
    
    /**
     * clean up.
     */
    public void cleanup() {
        serviceCompose.stop();
    }

    /**
     * get scenarioId.
     *
     * @return scenarioId
     */
    public String getScenarioId() {
        return scenarioId;
    }
}
