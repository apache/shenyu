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

import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.e2e.client.EnvironmentClient;
import org.apache.shenyu.e2e.client.ExternalServiceClient;
import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.common.IdGenerator;
import org.apache.shenyu.e2e.config.ServiceConfigure;
import org.apache.shenyu.e2e.enums.ServiceTypeEnum;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure;

import java.util.HashMap;
import java.util.Map;

public class ShenYuExtensionContext {

    private final String scenarioId;
    
    private final EnvironmentClient environmentClient = new EnvironmentClient();

    private Map<String, AdminClient> adminClientMap;

    private Map<String, GatewayClient> gatewayClientMap;

    private Map<String, ExternalServiceClient> externalServiceClientMap;

    /**
     * Instantiates a new Shenyu e2e extension context.
     */
    public ShenYuExtensionContext(final ShenYuEngineConfigure configure) {
        this.scenarioId = IdGenerator.generateScenarioId();
        Map<String, ServiceConfigure> serviceConfigureMap = configure.getServiceConfigureMap();
        serviceConfigureMap.forEach((serviceName, serviceConfigure) -> {
            if (ServiceTypeEnum.SHENYU_ADMIN.equals(serviceConfigure.getServiceType())) {
                if (MapUtils.isEmpty(adminClientMap)) {
                    adminClientMap = new HashMap<>();
                }
                AdminClient client = new AdminClient(scenarioId, serviceName, serviceConfigure.getBaseUrl(), serviceConfigure.getParameters());
                adminClientMap.put(serviceName, client);
                environmentClient.add(client);
            } else if (ServiceTypeEnum.SHENYU_GATEWAY.equals(serviceConfigure.getServiceType())) {
                if (MapUtils.isEmpty(gatewayClientMap)) {
                    gatewayClientMap = new HashMap<>();
                }
                GatewayClient client = new GatewayClient(scenarioId, serviceName, serviceConfigure.getBaseUrl(), serviceConfigure.getParameters());
                gatewayClientMap.put(serviceName, client);
                environmentClient.add(client);
            } else if (ServiceTypeEnum.EXTERNAL_SERVICE.equals(serviceConfigure.getServiceType())) {
                if (MapUtils.isEmpty(externalServiceClientMap)) {
                    externalServiceClientMap = new HashMap<>();
                }
                ExternalServiceClient client = new ExternalServiceClient(scenarioId, serviceName,
                        serviceConfigure.getBaseUrl(), serviceConfigure.getParameters());
                externalServiceClientMap.put(serviceName, client);
                environmentClient.add(client);
            }
        });
        
    }

    /**
     * Gets admin client map.
     *
     * @return current admin client map
     */
    public Map<String, AdminClient> getAdminClientMap() {
        return adminClientMap;
    }

    /**
     * Gets gateway client map.
     *
     * @return current gateway client map
     */
    public Map<String, GatewayClient> getGatewayClientMap() {
        return gatewayClientMap;
    }
    


    /**
     * Gets external service client map.
     *
     * @return current external service client map
     */
    public Map<String, ExternalServiceClient> getExternalServiceClientMap() {
        return externalServiceClientMap;
    }

    
    /**
     * Gets environment client.
     *
     * @return current environment client
     */
    public EnvironmentClient getEnvironmentClient() {
        return environmentClient;
    }
}
