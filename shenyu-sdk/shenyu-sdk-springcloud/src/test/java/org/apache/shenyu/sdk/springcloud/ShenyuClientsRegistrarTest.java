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

package org.apache.shenyu.sdk.springcloud;

import feign.RetryableException;
import feign.Target;
import java.util.Objects;
import java.util.Optional;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledForJreRange;
import org.junit.jupiter.api.condition.JRE;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.loadbalancer.Response;
import org.springframework.cloud.commons.httpclient.HttpClientConfiguration;
import org.springframework.cloud.openfeign.FeignAutoConfiguration;
import org.springframework.cloud.openfeign.FeignClientsConfiguration;
import org.springframework.cloud.openfeign.loadbalancer.FeignLoadBalancerAutoConfiguration;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import reactor.core.publisher.Mono;

/**
 * {@link ShenyuClientsRegistrar} test.
 */
public class ShenyuClientsRegistrarTest {

    @Test
    @DisabledForJreRange(min = JRE.JAVA_16)
    public void registerBeanDefinitionsTest() {
        AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();
        ((DefaultListableBeanFactory) context.getBeanFactory()).setAllowBeanDefinitionOverriding(false);
        context.scan("org.springframework.cloud.loadbalancer.config");
        context.register(FeignAutoConfiguration.class);
        context.register(FeignLoadBalancerAutoConfiguration.class);
        context.register(FeignClientsConfiguration.class);
        context.register(HttpClientConfiguration.class);
        context.register(ShenyuTestConfig.class);

        final RegisterConfig config = spy(RegisterConfig.class);
        when(config.getServerLists()).thenReturn("localhost:1234");
        final ShenyuDiscoveryClient shenyuDiscoveryClient = spy(new ShenyuDiscoveryClient(config));
        context.registerBean(ShenyuServiceInstanceLoadBalancer.class, shenyuDiscoveryClient);
        context.refresh();

        final ShenyuClientsRegistrarTest.ShenyuApiClient apiClient = context.getBean(ShenyuClientsRegistrarTest.ShenyuApiClient.class);

        Object invocationHandler = ReflectionTestUtils.getField(apiClient, "h");
        assertNotNull(invocationHandler);
        Target.HardCodedTarget factoryBean = (Target.HardCodedTarget) ReflectionTestUtils.getField(invocationHandler, "target");
        assertNotNull(factoryBean);
        assertEquals(factoryBean.name(), "shenyu-gateway");
        assertEquals(factoryBean.url(), "http://shenyu-gateway/dev/api");
        assertEquals(factoryBean.type(), ShenyuApiClient.class);

        assertThrows(RetryableException.class, () -> apiClient.findById("id"));
        verify(shenyuDiscoveryClient, times(1)).getInstances(anyString());

        final ShenyuServiceInstanceLoadBalancer instanceLoadBalancer = context.getBean(ShenyuServiceInstanceLoadBalancer.class);
        final Mono<Response<ServiceInstance>> chosen = instanceLoadBalancer.choose();
        final Optional<ServiceInstance> serviceInstance = chosen.map(Response::getServer).filter(instance -> Objects.equals("http://localhost:1234", instance.getInstanceId())).blockOptional();
        assertTrue(serviceInstance.isPresent(), "loadBalancer not work in local env.");
    }

    @Configuration
    @EnableShenyuClients(basePackageClasses = {ShenyuApiClient.class})
    public static class ShenyuTestConfig {

    }

    @ShenyuClient(value = "shenyuApiClient", path = "/dev/api")
    public interface ShenyuApiClient {

        /**
         * find by id mapping.
         * @param id id
         * @return MetaData
         */
        @GetMapping("/findById")
        MetaData findById(@RequestParam("id") String id);

        /**
         * insert mapping.
         * @param one one
         * @return Integer
         */
        @PostMapping("/insert")
        Integer insert(@RequestBody MetaData one);

        /**
         * update mapping.
         * @param one one
         * @return int
         */
        @PutMapping("/update")
        int update(MetaData one);

        /**
         * delete mapping.
         * @param id id
         * @return int
         */
        @DeleteMapping("/delete")
        int del(@RequestParam("id") String id);
    }
}
