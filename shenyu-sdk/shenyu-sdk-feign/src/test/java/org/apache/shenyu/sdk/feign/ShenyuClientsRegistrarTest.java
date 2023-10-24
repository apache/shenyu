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

package org.apache.shenyu.sdk.feign;

import com.google.common.collect.Maps;
import feign.Client;
import feign.Request;
import feign.Response;
import feign.Target;
import feign.codec.DecodeException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledForJreRange;
import org.junit.jupiter.api.condition.JRE;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration;
import org.springframework.cloud.openfeign.FeignAutoConfiguration;
import org.springframework.cloud.openfeign.FeignClientsConfiguration;
import org.springframework.cloud.openfeign.loadbalancer.FeignBlockingLoadBalancerClient;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * {@link ShenyuClientsRegistrar} test.
 */
public class ShenyuClientsRegistrarTest {

    private final Client delegate = mock(Client.class);

    private final FeignBlockingLoadBalancerClient feignBlockingLoadBalancerClient = mock(FeignBlockingLoadBalancerClient.class);

    @Test
    @DisabledForJreRange(min = JRE.JAVA_16)
    public void registerBeanDefinitionsTest() throws IOException {
        AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();
        ((DefaultListableBeanFactory) context.getBeanFactory()).setAllowBeanDefinitionOverriding(false);
        context.register(FeignAutoConfiguration.class);
        context.register(FeignClientsConfiguration.class);
        //context.register(HttpClientConfiguration.class);
        context.register(ShenyuTestConfig.class);
        context.register(HttpMessageConvertersAutoConfiguration.class);

        when(feignBlockingLoadBalancerClient.getDelegate()).thenReturn(delegate);
        context.registerBean(FeignBlockingLoadBalancerClient.class, () -> feignBlockingLoadBalancerClient);

        final RegisterConfig config = spy(RegisterConfig.class);
        when(config.getServerLists()).thenReturn("localhost:1234");

        final ShenyuDiscoveryClient shenyuDiscoveryClient = spy(new ShenyuDiscoveryClient(config));
        context.registerBean(ShenyuDiscoveryClient.class, () -> shenyuDiscoveryClient);
        context.refresh();

        final ShenyuClientsRegistrarTest.ShenyuApiClient apiClient = context.getBean(ShenyuClientsRegistrarTest.ShenyuApiClient.class);

        Object invocationHandler = ReflectionTestUtils.getField(apiClient, "h");
        assertNotNull(invocationHandler);
        Target.HardCodedTarget factoryBean = (Target.HardCodedTarget) ReflectionTestUtils.getField(invocationHandler, "target");
        assertNotNull(factoryBean);
        assertEquals(factoryBean.name(), "shenyuApiClient");
        assertEquals(factoryBean.url(), "http://shenyuApiClient/dev/api");
        assertEquals(factoryBean.type(), ShenyuApiClient.class);

        final Response respSpy = spy(Response.builder()
                                         .body("1", StandardCharsets.UTF_8)
                                         .status(HttpStatus.OK.value())
                                         .request(Request.create(Request.HttpMethod.POST, "/dev/null", Maps.newHashMap(), null, null, null))
                                         .build());
        when(delegate.execute(any(), any())).thenReturn(respSpy);
        assertThrowsExactly(DecodeException.class, () -> apiClient.del("id"));
        verify(delegate, times(1)).execute(any(), any());
        verify(shenyuDiscoveryClient, times(1)).getInstance(anyString());
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
        @DeleteMapping(value = "/delete", consumes = MediaType.TEXT_PLAIN_VALUE)
        int del(@RequestParam("id") String id);
    }
}
