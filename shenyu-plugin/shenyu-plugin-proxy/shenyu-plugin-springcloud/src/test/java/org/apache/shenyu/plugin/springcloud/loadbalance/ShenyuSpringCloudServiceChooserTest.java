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

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.config.ShenyuConfig.SpringCloudCacheConfig;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.springcloud.handler.SpringCloudPluginDataHandler;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * The Test Case For ShenyuSpringCloudLoadBalancerClientTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ShenyuSpringCloudServiceChooserTest {

    private ShenyuSpringCloudServiceChooser serviceChooser;

    private SpringCloudPluginDataHandler springCloudPluginDataHandler;

    private ShenyuInstanceRegisterRepository shenyuInstanceRegisterRepository;

    @BeforeEach
    public void setup() {
        this.mockSpringCloudConfig();
        ShenyuConfig.SpringCloudCacheConfig springCloudCacheConfig = SpringBeanUtils.getInstance().getBean(SpringCloudCacheConfig.class);
        ConfigurableApplicationContext context = (ConfigurableApplicationContext) SpringBeanUtils.getInstance().getApplicationContext();

        springCloudPluginDataHandler = new SpringCloudPluginDataHandler(springCloudCacheConfig, context.getEnvironment());

        shenyuInstanceRegisterRepository = mock(ShenyuInstanceRegisterRepository.class);
    }

    @Test
    public void testChoose() {
        final String ip = "0.0.0.0";
        final String selectorId = "1";
        final String loadbalancer = "roundRobin";

        // serviceInstance is null
        // mock return value
        when(serviceChooser.choose("test", selectorId, ip, loadbalancer)).thenReturn(null);
        Upstream upstreamIsNull = serviceChooser.choose("test", selectorId, ip, loadbalancer);
        Assertions.assertNull(upstreamIsNull);

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

        // mock data
        Upstream mockUpstream = Upstream.builder().url("localhost:8080").protocol("http").build();
        // mock return value
        when(serviceChooser.choose("serviceId", selectorId, ip, loadbalancer)).thenReturn(mockUpstream);
        Upstream upstream = serviceChooser.choose("serviceId", selectorId, ip, loadbalancer);
        Assertions.assertNotNull(upstream);
        Assertions.assertEquals(upstream.getUrl(), "localhost:8080");

        // gray flow
        springCloudSelectorHandle.setGray(true);
        final SelectorData selectorDataGray = SelectorData.builder()
                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
                .id("1")
                .build();
        springCloudPluginDataHandler.handlerSelector(selectorDataGray);

        // mock return value
        when(serviceChooser.choose("serviceId", selectorId, ip, loadbalancer)).thenReturn(mockUpstream);
        Upstream upstreamGray = serviceChooser.choose("serviceId", selectorId, ip, loadbalancer);
        Assertions.assertNotNull(upstreamGray);
        Assertions.assertEquals(upstreamGray.getUrl(), "localhost:8080");
    }

    @Test
    public void testLoadBalancer() {
        final String ip = "0.0.0.0";
        final String selectorId = "1";
        final String loadbalancer = "roundRobin";
        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudSelectorHandle.builder()
                .serviceId("serviceId")
                .gray(false)
                .build();
        final SelectorData selectorData = SelectorData.builder()
                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
                .id("1")
                .build();
        springCloudPluginDataHandler.handlerSelector(selectorData);

        final List<InstanceEntity> serviceInstances = new ArrayList<>();
        InstanceEntity defaultServiceInstance = new InstanceEntity();
        defaultServiceInstance.setUri(URI.create("http://localhost:8081"));
        defaultServiceInstance.setPort(8081);
        defaultServiceInstance.setHost("localhost");

        InstanceEntity defaultServiceInstance2 = new InstanceEntity();
        defaultServiceInstance2.setUri(URI.create("http://localhost:8080"));
        defaultServiceInstance2.setPort(8080);
        defaultServiceInstance2.setHost("localhost");
        serviceInstances.add(defaultServiceInstance);
        serviceInstances.add(defaultServiceInstance2);
        final ShenyuSpringCloudServiceChooser shenyuServiceChoose = new ShenyuSpringCloudServiceChooser();

        final MockedStatic<SpringCloudPluginDataHandler> springCloudPluginDataHandlerMockedStatic = mockStatic(SpringCloudPluginDataHandler.class);
        springCloudPluginDataHandlerMockedStatic.when(SpringCloudPluginDataHandler::getRepository).thenReturn(shenyuInstanceRegisterRepository);
        // mock return value
        when(shenyuInstanceRegisterRepository.selectInstances(any())).thenReturn(serviceInstances);

        Upstream upstream1 = shenyuServiceChoose.choose("serviceId", selectorId, ip, loadbalancer);
        Upstream upstream2 = shenyuServiceChoose.choose("serviceId", selectorId, ip, loadbalancer);
        // if roundRobin, upstream1 not equals upstream2
        Assertions.assertNotEquals(upstream1, upstream2);
    }

    private void mockSpringCloudConfig() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        // create mock object
        serviceChooser = mock(ShenyuSpringCloudServiceChooser.class);
        when(context.getBean(SpringCloudCacheConfig.class)).thenReturn(new SpringCloudCacheConfig());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }
}
