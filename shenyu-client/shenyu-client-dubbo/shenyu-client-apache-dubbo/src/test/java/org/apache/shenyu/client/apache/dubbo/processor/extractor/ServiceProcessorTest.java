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

package org.apache.shenyu.client.apache.dubbo.processor.extractor;

import org.apache.dubbo.config.ProtocolConfig;
import org.apache.dubbo.config.annotation.Service;
import org.apache.dubbo.config.spring.ServiceBean;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Method;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class ServiceProcessorTest {

    private ServiceProcessor serviceProcessor;

    @Mock
    private ServiceBean<?> serviceBean;

    @Mock
    private ProtocolConfig protocolConfig;

    @BeforeEach
    void setUp() {
        serviceProcessor = new ServiceProcessor();
    }

    @Test
    void testSupportedClient() {
        List<String> clients = serviceProcessor.supportedClient();
        assertEquals(1, clients.size());
        assertEquals(RpcTypeEnum.DUBBO.getName(), clients.get(0));
    }

    @Test
    void testMatchAnnotation() {
        assertEquals(Service.class, serviceProcessor.matchAnnotation());
    }

    @Test
    void testProcessWithServiceBeanSetsBeanPathAndRpcExt() throws Exception {
        when(serviceBean.getProtocol()).thenReturn(protocolConfig);
        when(protocolConfig.getName()).thenReturn("dubbo");
        when(serviceBean.getGroup()).thenReturn("testGroup");
        when(serviceBean.getVersion()).thenReturn("1.0.0");
        when(serviceBean.getLoadbalance()).thenReturn("random");
        when(serviceBean.getRetries()).thenReturn(3);
        when(serviceBean.getTimeout()).thenReturn(1000);
        when(serviceBean.getSent()).thenReturn(true);
        when(serviceBean.getCluster()).thenReturn("failover");
        when(serviceBean.getSerialization()).thenReturn("hessian2");
        when(serviceBean.getMethods()).thenReturn(null);

        ApiBean apiBean = new ApiBean(RpcTypeEnum.DUBBO.getName(), "testBean", serviceBean);

        Service serviceAnnotation = TestServiceBean.class.getAnnotation(Service.class);
        serviceProcessor.process(apiBean, serviceAnnotation);

        assertEquals(serviceAnnotation.path(), apiBean.getBeanPath());
        String rpcExt = apiBean.getPropertiesValue("rpcExt");
        assertNotNull(rpcExt);
        assertTrue(rpcExt.contains("\"serialization\":\"hessian2\""));
        assertTrue(rpcExt.contains("\"protocol\":\"dubbo\""));
        assertTrue(rpcExt.contains("\"group\":\"testGroup\""));
    }

    @Test
    void testProcessWithNonServiceBeanReturnsEmptyRpcExt() {
        Object plainBean = new Object();
        ApiBean apiBean = new ApiBean(RpcTypeEnum.DUBBO.getName(), "testBean", plainBean);

        Service serviceAnnotation = TestServiceBean.class.getAnnotation(Service.class);
        serviceProcessor.process(apiBean, serviceAnnotation);

        String rpcExt = apiBean.getPropertiesValue("rpcExt");
        assertEquals("{}", rpcExt);
    }

    @Test
    void testProcessApiDefinitionWithServiceBean() throws Exception {
        when(serviceBean.getProtocol()).thenReturn(protocolConfig);
        when(protocolConfig.getName()).thenReturn("dubbo");
        when(serviceBean.getGroup()).thenReturn("myGroup");
        when(serviceBean.getVersion()).thenReturn("2.0.0");
        when(serviceBean.getLoadbalance()).thenReturn(null);
        when(serviceBean.getRetries()).thenReturn(null);
        when(serviceBean.getTimeout()).thenReturn(null);
        when(serviceBean.getSent()).thenReturn(null);
        when(serviceBean.getCluster()).thenReturn(null);
        when(serviceBean.getSerialization()).thenReturn("fastjson");
        when(serviceBean.getMethods()).thenReturn(null);

        ApiBean apiBean = new ApiBean(RpcTypeEnum.DUBBO.getName(), "testBean", serviceBean);
        Method method = TestServiceBean.class.getMethod("testMethod");
        ApiBean.ApiDefinition definition = new ApiBean.ApiDefinition(apiBean, method);

        serviceProcessor.process(definition);

        String rpcExt = definition.getPropertiesValue("rpcExt");
        assertNotNull(rpcExt);
        assertTrue(rpcExt.contains("\"serialization\":\"fastjson\""));
        assertTrue(rpcExt.contains("\"protocol\":\"dubbo\""));
        assertTrue(rpcExt.contains("\"group\":\"myGroup\""));
        assertTrue(rpcExt.contains("\"version\":\"2.0.0\""));
    }

    @Test
    void testProcessApiDefinitionWithNonServiceBean() throws Exception {
        Object plainBean = new Object();
        ApiBean apiBean = new ApiBean(RpcTypeEnum.DUBBO.getName(), "testBean", plainBean);
        Method method = TestServiceBean.class.getMethod("testMethod");
        ApiBean.ApiDefinition definition = new ApiBean.ApiDefinition(apiBean, method);

        serviceProcessor.process(definition);

        String rpcExt = definition.getPropertiesValue("rpcExt");
        assertEquals("{}", rpcExt);
    }

    @Test
    void testProcessWithNullProtocol() {
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

        ApiBean apiBean = new ApiBean(RpcTypeEnum.DUBBO.getName(), "testBean", serviceBean);

        Service serviceAnnotation = TestServiceBean.class.getAnnotation(Service.class);
        serviceProcessor.process(apiBean, serviceAnnotation);

        String rpcExt = apiBean.getPropertiesValue("rpcExt");
        assertNotNull(rpcExt);
        assertTrue(rpcExt.contains("\"protocol\":\"\""));
    }

    @Service(path = "/test-path")
    private static class TestServiceBean {
        public void testMethod() {
        }
    }
}
