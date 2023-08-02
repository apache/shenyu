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

package org.apache.shenyu.sdk.spring.proxy;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Map;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;
import org.apache.shenyu.sdk.spring.EnableShenyuClients;
import org.apache.shenyu.sdk.spring.ShenyuClient;
import org.apache.shenyu.sdk.spring.support.SpringMvcContract;
import static org.mockito.Mockito.spy;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * AbstractProxyTest.
 */
public abstract class AbstractProxyTest {

    protected static final Field METHOD_HANDLER_MAP;

    protected static final Field REQUEST_TEMPLATE;

    protected static final Field PROXY_CACHE;

    static {
        try {
            METHOD_HANDLER_MAP = ShenyuClientInvocationHandler.class.getDeclaredField("methodHandlerMap");
            METHOD_HANDLER_MAP.setAccessible(true);
            REQUEST_TEMPLATE = ShenyuClientMethodHandler.class.getDeclaredField("requestTemplate");
            REQUEST_TEMPLATE.setAccessible(true);
            PROXY_CACHE = ShenyuClientProxyFactory.class.getDeclaredField("PROXY_CACHE");
            PROXY_CACHE.setAccessible(true);
        } catch (NoSuchFieldException e) {
            throw new RuntimeException(e);
        }
    }

    private final MetaData metaData = MetaData.builder().build();

    private ShenyuSdkClient client;

    private AbstractProxyTest.InvocationClient topClient;

    private Map<Method, ShenyuClientMethodHandler> map;

    /**
     * proxy test protect method.
     * @throws IllegalAccessException IllegalAccessException
     * @throws IOException            IOException
     */
    @SuppressWarnings("unchecked")
    public void init() throws IllegalAccessException, IOException {
        AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();
        client = spy(ShenyuSdkClient.class);
        ((DefaultListableBeanFactory) context.getBeanFactory()).setAllowBeanDefinitionOverriding(false);
        context.register(AbstractProxyTest.TestConfig.class);
        context.register(SpringMvcContract.class);
        context.registerBean("shenyuSdkClient", ShenyuSdkClient.class, () -> client);

        context.refresh();

        topClient = context.getBean(AbstractProxyTest.InvocationClient.class);
        ShenyuClientInvocationHandler invocationHandler = (ShenyuClientInvocationHandler) ReflectionTestUtils.getField(topClient, "h");
        map = (Map<Method, ShenyuClientMethodHandler>) METHOD_HANDLER_MAP.get(invocationHandler);
    }

    /**
     * get meta data.
     * @return MetaData
     */
    public MetaData getMetaData() {
        return metaData;
    }

    /**
     * get client.
     * @return ShenyuSdkClient
     */
    public ShenyuSdkClient getClient() {
        return client;
    }

    /**
     * get invocation client.
     * @return InvocationClient
     */
    public InvocationClient getTopClient() {
        return topClient;
    }

    /**
     * get method handler map.
     * @return Map&lt;Method, ShenyuClientMethodHandler&gt;
     */
    public Map<Method, ShenyuClientMethodHandler> getMap() {
        return map;
    }

    @EnableShenyuClients(basePackageClasses = AbstractProxyTest.InvocationClient.class)
    public static class TestConfig {

    }

    @ShenyuClient(value = "invocationClient", path = "/dev/null")
    public interface InvocationClient {

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
