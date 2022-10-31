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

package org.apache.shenyu.springboot.starter.gateway;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.boot.web.embedded.netty.NettyReactiveWebServerFactory;
import org.springframework.boot.web.reactive.error.DefaultErrorAttributes;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.support.DefaultServerCodecConfigurer;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.WebFilter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link ShenyuConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class ShenyuConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuConfiguration.class))
            .withBean(ShenyuConfigurationTest.class)
            .withBean(DefaultServerCodecConfigurer.class)
            .withBean(DefaultErrorAttributes.class)
            .withPropertyValues(
                "debug=true",
                "shenyu.cross.enabled=true",
                "shenyu.file.enabled=true",
                "shenyu.exclude.enabled=true",
                "shenyu.local.enabled=true"
            );
    }

    @Test
    public void testShenyuWebHandler() {
        applicationContextRunner.run(context -> {
                ShenyuWebHandler handler = context.getBean("webHandler", ShenyuWebHandler.class);
                assertNotNull(handler);
            }
        );
    }

    @Test
    public void testDispatcherHandler() {
        applicationContextRunner.run(context -> {
                DispatcherHandler handler = context.getBean("dispatcherHandler", DispatcherHandler.class);
                assertNotNull(handler);
            }
        );
    }

    @Test
    public void testRpcParamTransformPlugin() {
        applicationContextRunner.run(context -> {
                ShenyuPlugin plugin = context.getBean("paramTransformPlugin", ShenyuPlugin.class);
                assertNotNull(plugin);
            }
        );
    }

    @Test
    public void testCommonPluginDataSubscriber() {
        applicationContextRunner.run(context -> {
                PluginDataSubscriber subscriber = context.getBean("pluginDataSubscriber", PluginDataSubscriber.class);
                assertNotNull(subscriber);
            }
        );
    }

    @Test
    public void testShenyuLoaderService() {
        applicationContextRunner.run(context -> {
                ShenyuLoaderService service = context.getBean("shenyuLoaderService", ShenyuLoaderService.class);
                assertNotNull(service);
            }
        );
    }

    @Test
    public void testForwardedRemoteAddressResolver() {
        applicationContextRunner.run(context -> {
                RemoteAddressResolver resolver = context.getBean("remoteAddressResolver", RemoteAddressResolver.class);
                assertNotNull(resolver);
            }
        );
    }

    @Test
    public void testWebFilter() {
        applicationContextRunner.run(context -> {
                WebFilter localDispatcherFilter = context.getBean("localDispatcherFilter", WebFilter.class);
                assertNotNull(localDispatcherFilter);
                WebFilter crossFilter = context.getBean("crossFilter", WebFilter.class);
                assertNotNull(crossFilter);
                WebFilter fileSizeFilter = context.getBean("fileSizeFilter", WebFilter.class);
                assertNotNull(fileSizeFilter);
                WebFilter excludeFilter = context.getBean("excludeFilter", WebFilter.class);
                assertNotNull(excludeFilter);
            }
        );
    }

    @Test
    public void testShenyuConfig() {
        applicationContextRunner.run(context -> {
                ShenyuConfig config = context.getBean("shenyuConfig", ShenyuConfig.class);
                assertNotNull(config);
            }
        );
    }

    @Test
    public void testNettyReactiveWebServerFactory() {
        applicationContextRunner.run(context -> {
            NettyReactiveWebServerFactory config = context.getBean("nettyReactiveWebServerFactory", NettyReactiveWebServerFactory.class);
            assertNotNull(config);
        });
    }

    @Test
    public void testClass() {
        applicationContextRunner.run(context -> {
            ShenyuResult<?> shenyuResult = context.getBean(ShenyuResult.class);
            assertEquals(CustomShenyuResultConfig.CustomShenyuResult.class, shenyuResult.getClass());
        });
    }

    @Configuration
    static class CustomShenyuResultConfig {

        @Bean
        public ShenyuResult<?> shenyuResult() {
            return new CustomShenyuResult();
        }

        static class CustomShenyuResult implements ShenyuResult<Object> {

            @Override
            public Object error(final int code, final String message, final Object object) {
                return null;
            }
        }
    }
}
