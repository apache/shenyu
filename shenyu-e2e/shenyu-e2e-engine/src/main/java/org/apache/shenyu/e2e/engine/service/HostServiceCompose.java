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

package org.apache.shenyu.e2e.engine.service;

import com.google.common.collect.Lists;
import junit.framework.AssertionFailedError;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.apache.shenyu.e2e.client.ExternalServiceClient;
import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.HostConfigure;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.HostConfigure.HostServiceConfigure;

import java.util.List;
import java.util.Objects;

@Getter
@AllArgsConstructor
public class HostServiceCompose implements ServiceCompose {
    
    private HostConfigure configure;
    
    public void start() {
        List<HostServiceConfigure> configures = Lists.newArrayList(configure.getExternalServices());
        if (Objects.nonNull(configure.getAdmin())) {
            configures.add(configure.getAdmin());
        }
        if (Objects.nonNull(configure.getGateway())) {
            configures.add(configure.getGateway());
        }
        NamingResolver.INSTANCE.ofHostConfigure(configures);
    }
    
    @Override
    public AdminClient newAdminClient(String scenarioId) {
        HostServiceConfigure adminConfigure = configure.getAdmin();
        return new AdminClient(scenarioId, adminConfigure.getBaseUrl(), adminConfigure.getProperties());
    }
    
    @Override
    public GatewayClient newGatewayClient(String scenarioId) {
        HostServiceConfigure gatewayConfigure = configure.getGateway();
        return new GatewayClient(scenarioId, gatewayConfigure.getBaseUrl(), gatewayConfigure.getProperties());
    }
    
    @Override
    public ExternalServiceClient newExternalServiceClient(String externalServiceName) {
        HostServiceConfigure serviceConfigure = configure.getExternalServices().stream()
                .filter(e -> externalServiceName.equals(e.getServiceName()))
                .findFirst()
                .orElseThrow(() -> new AssertionFailedError("ExternalServiceClient[" + externalServiceName + "] configure: not found"));
        return new ExternalServiceClient(serviceConfigure.getBaseUrl(), serviceConfigure.getProperties());
    }
    
    public void stop() {
    
    }
}
