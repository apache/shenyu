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

import org.apache.shenyu.common.config.ShenyuConfig.SpringCloudCacheConfig;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.springcloud.handler.SpringCloudPluginDataHandler;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.core.ShenyuInstanceRegisterRepositoryFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.cloud.client.DefaultServiceInstance;
import org.springframework.context.ConfigurableApplicationContext;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * The Test Case For ShenyuSpringCloudLoadBalancerClientTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ShenyuSpringCloudServiceChooserTest {

    private ShenyuSpringCloudServiceChooser serviceChooser;

    private SpringCloudPluginDataHandler springCloudPluginDataHandler;

    @BeforeEach
    public void setup() {
        this.mockSpringCloudConfig();
//        final List<DefaultServiceInstance> serviceInstanceList = new ArrayList<>();
//        DefaultServiceInstance defaultServiceInstance = new DefaultServiceInstance();
//        defaultServiceInstance.setServiceId("serviceId");
//        defaultServiceInstance.setUri(URI.create("http://localhost:8080"));
//        defaultServiceInstance.setInstanceId("serviceId");
//        defaultServiceInstance.setPort(8080);
//        defaultServiceInstance.setHost("localhost");
//        serviceInstanceList.add(defaultServiceInstance);
//        SimpleDiscoveryProperties simpleDiscoveryProperties = new SimpleDiscoveryProperties();
//        Map<String, List<DefaultServiceInstance>> serviceInstanceMap = new HashMap<>();
//        serviceInstanceMap.put(defaultServiceInstance.getInstanceId(), serviceInstanceList);
//        simpleDiscoveryProperties.setInstances(serviceInstanceMap);

        RegisterConfig registerConfig = SpringBeanUtils.getInstance().getBean(RegisterConfig.class);
//        final ShenyuInstanceRegisterRepository repository = ShenyuInstanceRegisterRepositoryFactory.newAndInitInstance(registerConfig);
//        InstanceEntity instanceEntity = new InstanceEntity();
//        instanceEntity.setAppName("serviceId");
//        instanceEntity.setUri(URI.create("http://localhost:8080"));
//        instanceEntity.setHost("localhost");
//        instanceEntity.setPort(8080);
//        repository.persistInstance(instanceEntity);
//        // SimpleDiscoveryClient discoveryClient = new SimpleDiscoveryClient(simpleDiscoveryProperties);
//        serviceChooser = new ShenyuSpringCloudServiceChooser(registerConfig);
//        SpringCloudCacheConfig springCloudCacheConfig = SpringBeanUtils.getInstance().getBean(SpringCloudCacheConfig.class);
//        springCloudPluginDataHandler = new SpringCloudPluginDataHandler(registerConfig, springCloudCacheConfig);
    }

    @Test
    public void testChoose() throws InterruptedException {
        final String ip = "0.0.0.0";
        final String selectorId = "1";
        final String loadbalancer = "roundRobin";

        // th process of register instance needs some time
//        Thread.sleep(10000);
//
//        // serviceInstance is null
//        Upstream upstreamIsNull = serviceChooser.choose("test", selectorId, ip, loadbalancer);
//        Assertions.assertNull(upstreamIsNull);
//
//        // not gray flow
//        List<DivideUpstream> divideUpstreams = new ArrayList<>();
//        DivideUpstream divideUpstream = DivideUpstream.builder()
//                .upstreamUrl("localhost:8080")
//                .build();
//        divideUpstreams.add(divideUpstream);
//        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudSelectorHandle.builder()
//                .serviceId("serviceId")
//                .divideUpstreams(divideUpstreams)
//                .gray(false)
//                .build();
//        final SelectorData selectorData = SelectorData.builder()
//                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
//                .id("1")
//                .build();
//        springCloudPluginDataHandler.handlerSelector(selectorData);
//        Upstream upstream = serviceChooser.choose("serviceId", selectorId, ip, loadbalancer);
//        Assertions.assertNotNull(upstream);
//        Assertions.assertEquals(upstream.getUrl(), "localhost:8080");
//
//        // gray flow
//        springCloudSelectorHandle.setGray(true);
//        final SelectorData selectorDataGray = SelectorData.builder()
//                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
//                .id("1")
//                .build();
//        springCloudPluginDataHandler.handlerSelector(selectorDataGray);
//        Upstream upstreamGray = serviceChooser.choose("serviceId", selectorId, ip, loadbalancer);
//        Assertions.assertNotNull(upstreamGray);
//        Assertions.assertEquals(upstreamGray.getUrl(), "localhost:8080");
    }

    @Test
    public void testLoadBalancer() throws InterruptedException {
        final List<DefaultServiceInstance> serviceInstances = new ArrayList<>();
//        DefaultServiceInstance defaultServiceInstance = new DefaultServiceInstance();
//        defaultServiceInstance.setServiceId("serviceId");
//        defaultServiceInstance.setUri(URI.create("http://localhost:8081"));
//        defaultServiceInstance.setInstanceId("serviceId");
//        defaultServiceInstance.setPort(8081);
//        defaultServiceInstance.setHost("localhost");
//
//        DefaultServiceInstance defaultServiceInstance2 = new DefaultServiceInstance();
//        defaultServiceInstance2.setServiceId("serviceId");
//        defaultServiceInstance2.setUri(URI.create("http://localhost:8080"));
//        defaultServiceInstance2.setInstanceId("serviceId");
//        defaultServiceInstance2.setPort(8080);
//        defaultServiceInstance2.setHost("localhost");
//        serviceInstances.add(defaultServiceInstance);
//        serviceInstances.add(defaultServiceInstance2);
//
//        SimpleDiscoveryProperties simpleDiscoveryProperties = new SimpleDiscoveryProperties();
//        Map<String, List<DefaultServiceInstance>> serviceInstanceMap = new HashMap<>();
//        serviceInstanceMap.put(defaultServiceInstance.getInstanceId(), serviceInstances);
//        simpleDiscoveryProperties.setInstances(serviceInstanceMap);
        RegisterConfig registerConfig = SpringBeanUtils.getInstance().getBean(RegisterConfig.class);
//        final ShenyuInstanceRegisterRepository repository = ShenyuInstanceRegisterRepositoryFactory.newInstance(registerConfig.getRegisterType());
//        InstanceEntity instanceEntity1 = new InstanceEntity();
//        instanceEntity1.setAppName("serviceId");
//        instanceEntity1.setUri(URI.create("http://localhost:8080"));
//        instanceEntity1.setHost("localhost");
//        instanceEntity1.setPort(8080);
//        repository.persistInstance(instanceEntity1);

//        InstanceEntity instanceEntity2 = new InstanceEntity();
//        instanceEntity2.setAppName("serviceId");
//        instanceEntity2.setUri(URI.create("http://localhost:8081"));
//        instanceEntity2.setHost("localhost");
//        instanceEntity2.setPort(8081);
//        repository.persistInstance(instanceEntity2);
//
//        Thread.sleep(10000);

//        final SimpleDiscoveryClient simpleDiscoveryClient = new SimpleDiscoveryClient(simpleDiscoveryProperties);
//        final ShenyuSpringCloudServiceChooser shenyuServiceChoose = new ShenyuSpringCloudServiceChooser(simpleDiscoveryClient);

//        final String ip = "0.0.0.0";
//        final String selectorId = "1";
//        final String loadbalancer = "roundRobin";
//        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudSelectorHandle.builder()
//                .serviceId("serviceId")
//                .gray(false)
//                .build();
//        final SelectorData selectorData = SelectorData.builder()
//                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
//                .id("1")
//                .build();
//        springCloudPluginDataHandler.handlerSelector(selectorData);
//        Upstream upstream1 = serviceChooser.choose("serviceId", selectorId, ip, loadbalancer);
//        Upstream upstream2 = serviceChooser.choose("serviceId", selectorId, ip, loadbalancer);
//        // if roundRobin, upstream1 not equals upstream2
//        Assertions.assertNotEquals(upstream1, upstream2);
    }
    
    private void mockSpringCloudConfig() {
        final ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        RegisterConfig registerConfig = new RegisterConfig();
        registerConfig.setRegisterType("nacos");
        registerConfig.setEnabled(true);
        registerConfig.setServerLists("localhost:8848");
        Properties properties = new Properties();
        properties.setProperty("namespace", "ShenyuRegisterCenter");
        registerConfig.setProps(properties);
        when(context.getBean(SpringCloudCacheConfig.class)).thenReturn(new SpringCloudCacheConfig());
        when(context.getBean(RegisterConfig.class)).thenReturn(registerConfig);
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }
}
