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

package org.apache.shenyu.client.dubbo.common.dto;

import org.apache.dubbo.config.MethodConfig;
import org.apache.dubbo.config.ProtocolConfig;
import org.apache.dubbo.config.spring.ServiceBean;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class DubboRpcExtBuildersTest {

    @Mock
    private ServiceBean<?> serviceBean;

    @Mock
    private ProtocolConfig protocolConfig;

    @Test
    void testBuildRpcExtWithFullConfig() {
        when(serviceBean.getProtocol()).thenReturn(protocolConfig);
        when(protocolConfig.getName()).thenReturn("dubbo");
        when(serviceBean.getGroup()).thenReturn("testGroup");
        when(serviceBean.getVersion()).thenReturn("1.0.0");
        when(serviceBean.getLoadbalance()).thenReturn("random");
        when(serviceBean.getRetries()).thenReturn(3);
        when(serviceBean.getTimeout()).thenReturn(2000);
        when(serviceBean.getSent()).thenReturn(true);
        when(serviceBean.getCluster()).thenReturn("failfast");
        when(serviceBean.getSerialization()).thenReturn("hessian2");
        when(serviceBean.getMethods()).thenReturn(null);

        String result = DubboRpcExtBuilders.buildRpcExt(serviceBean);
        assertNotNull(result);
        assertTrue(result.contains("\"serialization\":\"hessian2\""));
        assertTrue(result.contains("\"protocol\":\"dubbo\""));
        assertTrue(result.contains("\"group\":\"testGroup\""));
        assertTrue(result.contains("\"version\":\"1.0.0\""));
        assertTrue(result.contains("\"loadbalance\":\"random\""));
        assertTrue(result.contains("\"retries\":3"));
        assertTrue(result.contains("\"timeout\":2000"));
        assertTrue(result.contains("\"sent\":true"));
        assertTrue(result.contains("\"cluster\":\"failfast\""));
    }

    @Test
    void testBuildRpcExtWithNullProtocol() {
        when(serviceBean.getProtocol()).thenReturn(null);
        when(serviceBean.getGroup()).thenReturn(null);
        when(serviceBean.getVersion()).thenReturn(null);
        when(serviceBean.getLoadbalance()).thenReturn(null);
        when(serviceBean.getRetries()).thenReturn(null);
        when(serviceBean.getTimeout()).thenReturn(null);
        when(serviceBean.getSent()).thenReturn(null);
        when(serviceBean.getCluster()).thenReturn(null);
        when(serviceBean.getSerialization()).thenReturn(null);
        when(serviceBean.getMethods()).thenReturn(null);

        String result = DubboRpcExtBuilders.buildRpcExt(serviceBean);
        assertNotNull(result);
        assertTrue(result.contains("\"protocol\":\"\""));
    }

    @Test
    void testBuildRpcExtWithMethodConfigs() {
        when(serviceBean.getProtocol()).thenReturn(protocolConfig);
        when(protocolConfig.getName()).thenReturn("dubbo");
        when(serviceBean.getGroup()).thenReturn("testGroup");
        when(serviceBean.getVersion()).thenReturn("1.0.0");
        when(serviceBean.getLoadbalance()).thenReturn("random");
        when(serviceBean.getRetries()).thenReturn(3);
        when(serviceBean.getTimeout()).thenReturn(2000);
        when(serviceBean.getSent()).thenReturn(true);
        when(serviceBean.getCluster()).thenReturn("failover");
        when(serviceBean.getSerialization()).thenReturn("hessian2");

        MethodConfig methodConfig = new MethodConfig();
        methodConfig.setName("sayHello");
        methodConfig.setLoadbalance("roundrobin");
        methodConfig.setRetries(5);
        methodConfig.setTimeout(3000);
        methodConfig.setSent(false);

        List<MethodConfig> methods = new ArrayList<>();
        methods.add(methodConfig);
        when(serviceBean.getMethods()).thenReturn(methods);

        String result = DubboRpcExtBuilders.buildRpcExt(serviceBean);
        assertNotNull(result);
        assertTrue(result.contains("\"methods\":[{"));
        assertTrue(result.contains("\"name\":\"sayHello\""));
        assertTrue(result.contains("\"loadbalance\":\"roundrobin\""));
        assertTrue(result.contains("\"retries\":5"));
        assertTrue(result.contains("\"timeout\":3000"));
    }

    @Test
    void testBuildRpcExtWithEmptyProtocolName() {
        when(serviceBean.getProtocol()).thenReturn(protocolConfig);
        when(protocolConfig.getName()).thenReturn("");
        when(serviceBean.getGroup()).thenReturn(null);
        when(serviceBean.getVersion()).thenReturn(null);
        when(serviceBean.getLoadbalance()).thenReturn(null);
        when(serviceBean.getRetries()).thenReturn(null);
        when(serviceBean.getTimeout()).thenReturn(null);
        when(serviceBean.getSent()).thenReturn(null);
        when(serviceBean.getCluster()).thenReturn(null);
        when(serviceBean.getSerialization()).thenReturn(null);
        when(serviceBean.getMethods()).thenReturn(null);

        String result = DubboRpcExtBuilders.buildRpcExt(serviceBean);
        assertNotNull(result);
        assertTrue(result.contains("\"protocol\":\"\""));
    }
}
