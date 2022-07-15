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

package org.apache.shenyu.plugin.springcloud.loadbalance;

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.springcloud.handler.SpringCloudPluginDataHandler;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.client.DefaultServiceInstance;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.simple.SimpleDiscoveryClient;
import org.springframework.cloud.client.discovery.simple.SimpleDiscoveryProperties;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The Test Case For ShenyuSpringCloudLoadBalancerClientTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ShenyuSpringCloudServiceChooserTest {

    private ShenyuSpringCloudServiceChooser serviceChooser;

    private final SpringCloudPluginDataHandler springCloudPluginDataHandler = new SpringCloudPluginDataHandler();

    @BeforeEach
    public void setup() {
        final List<DefaultServiceInstance> serviceInstanceList = new ArrayList<>();
        DefaultServiceInstance defaultServiceInstance = new DefaultServiceInstance();
        defaultServiceInstance.setServiceId("serviceId");
        defaultServiceInstance.setUri(URI.create("http://localhost:8080"));
        defaultServiceInstance.setInstanceId("serviceId");
        defaultServiceInstance.setPort(8080);
        defaultServiceInstance.setHost("localhost");
        serviceInstanceList.add(defaultServiceInstance);
        SimpleDiscoveryProperties simpleDiscoveryProperties = new SimpleDiscoveryProperties();
        Map<String, List<DefaultServiceInstance>> serviceInstanceMap = new HashMap<>();
        serviceInstanceMap.put(defaultServiceInstance.getInstanceId(), serviceInstanceList);
        simpleDiscoveryProperties.setInstances(serviceInstanceMap);
        SimpleDiscoveryClient discoveryClient = new SimpleDiscoveryClient(simpleDiscoveryProperties);
        serviceChooser = new ShenyuSpringCloudServiceChooser(discoveryClient);
    }

    @Test
    public void testChoose() {
        LoadBalanceKey loadBalanceKey = buildDefaultLoadBalanceKey();

        LoadBalanceKeyHolder.setLoadBalanceKey(loadBalanceKey);

        // serviceInstance is null
        ServiceInstance serviceInstanceIsNull = serviceChooser.choose("test");
        Assertions.assertNull(serviceInstanceIsNull);

        // not gray flow
        List<DivideUpstream> divideUpstreams = new ArrayList<>();
        DivideUpstream divideUpstream = DivideUpstream.builder()
                .upstreamUrl("localhost:8080")
                .build();
        divideUpstreams.add(divideUpstream);
        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudSelectorHandle.builder()
                .serviceId("serviceId")
                .divideUpstreams(divideUpstreams)
                .gray(false)
                .build();
        final SelectorData selectorData = SelectorData.builder()
                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
                .id("1")
                .build();
        springCloudPluginDataHandler.handlerSelector(selectorData);
        ServiceInstance serviceInstance = serviceChooser.choose("serviceId");
        Assertions.assertNotNull(serviceInstance);
        Assertions.assertEquals(serviceInstance.getInstanceId(), "serviceId");

        // gray flow
        springCloudSelectorHandle.setGray(true);
        final SelectorData selectorDataGray = SelectorData.builder()
                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
                .id("1")
                .build();
        springCloudPluginDataHandler.handlerSelector(selectorDataGray);
        ServiceInstance serviceInstanceGray = serviceChooser.choose("serviceId");
        Assertions.assertNotNull(serviceInstanceGray);
        Assertions.assertEquals(serviceInstanceGray.getHost(), "localhost");
    }

    @Test
    public void testLoadBalancer() {
        final List<DefaultServiceInstance> serviceInstances = new ArrayList<>();
        DefaultServiceInstance defaultServiceInstance = new DefaultServiceInstance();
        defaultServiceInstance.setServiceId("serviceId");
        defaultServiceInstance.setUri(URI.create("http://localhost:8081"));
        defaultServiceInstance.setInstanceId("serviceId");
        defaultServiceInstance.setPort(8081);
        defaultServiceInstance.setHost("localhost");

        DefaultServiceInstance defaultServiceInstance2 = new DefaultServiceInstance();
        defaultServiceInstance2.setServiceId("serviceId");
        defaultServiceInstance2.setUri(URI.create("http://localhost:8080"));
        defaultServiceInstance2.setInstanceId("serviceId");
        defaultServiceInstance2.setPort(8080);
        defaultServiceInstance2.setHost("localhost");
        serviceInstances.add(defaultServiceInstance);
        serviceInstances.add(defaultServiceInstance2);

        SimpleDiscoveryProperties simpleDiscoveryProperties = new SimpleDiscoveryProperties();
        Map<String, List<DefaultServiceInstance>> serviceInstanceMap = new HashMap<>();
        serviceInstanceMap.put(defaultServiceInstance.getInstanceId(), serviceInstances);
        simpleDiscoveryProperties.setInstances(serviceInstanceMap);
        final SimpleDiscoveryClient simpleDiscoveryClient = new SimpleDiscoveryClient(simpleDiscoveryProperties);
        final ShenyuSpringCloudServiceChooser shenyuServiceChoose = new ShenyuSpringCloudServiceChooser(simpleDiscoveryClient);

        LoadBalanceKey loadBalanceKey = buildDefaultLoadBalanceKey();
        LoadBalanceKeyHolder.setLoadBalanceKey(loadBalanceKey);
        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudSelectorHandle.builder()
                .serviceId("serviceId")
                .gray(false)
                .build();
        final SelectorData selectorData = SelectorData.builder()
                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
                .id("1")
                .build();
        springCloudPluginDataHandler.handlerSelector(selectorData);
        ServiceInstance serviceInstance = shenyuServiceChoose.choose("serviceId");
        ServiceInstance serviceInstance2 = shenyuServiceChoose.choose("serviceId");
        // if roundRobin, serviceInstance not equals serviceInstance2
        Assertions.assertNotEquals(serviceInstance, serviceInstance2);
    }

    private static LoadBalanceKey buildDefaultLoadBalanceKey() {
        LoadBalanceKey loadBalanceKey = new LoadBalanceKey();
        loadBalanceKey.setIp("0.0.0.0");
        loadBalanceKey.setSelectorId("1");
        loadBalanceKey.setLoadBalance("roundRobin");
        LoadBalanceKeyHolder.setLoadBalanceKey(loadBalanceKey);
        return loadBalanceKey;
    }
}
