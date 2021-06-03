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

package org.apache.shenyu.springboot.starter.plugin.springcloud;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.junit.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.cloud.client.loadbalancer.LoadBalancerClient;
import org.springframework.cloud.netflix.ribbon.RibbonAutoConfiguration;
import org.springframework.cloud.netflix.ribbon.RibbonClientConfiguration;
import org.springframework.cloud.netflix.ribbon.RibbonLoadBalancerClient;
import org.springframework.web.reactive.DispatcherHandler;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * test case for {@link SpringCloudPluginConfiguration}.
 */
public class SpringCloudPluginConfigurationTest {

    @Test
    public void testSpringCloudPlugin() {
        new ApplicationContextRunner()
            .withConfiguration(
                AutoConfigurations.of(
                        RibbonAutoConfiguration.class,
                        DispatcherHandler.class,
                        RibbonClientConfiguration.class,
                        SpringCloudPluginConfiguration.class
                ))
            .withPropertyValues(
                "debug=true",
                "spring.main.banner-mode=off",
                "ribbon.client.name=test")
            .run(
                context -> {
                    assertThat(context).hasSingleBean(LoadBalancerClient.class);
                    assertThat(context).hasSingleBean(RibbonAutoConfiguration.class);
                    assertThat(context).hasSingleBean(DispatcherHandler.class);
                    assertThat(context).getBean(LoadBalancerClient.class).isInstanceOf(RibbonLoadBalancerClient.class);
                    ShenyuPlugin plugin = context.getBean(ShenyuPlugin.class);
                    assertThat(plugin.named()).isEqualTo(PluginEnum.SPRING_CLOUD.getName());
                }
            );
    }
}
