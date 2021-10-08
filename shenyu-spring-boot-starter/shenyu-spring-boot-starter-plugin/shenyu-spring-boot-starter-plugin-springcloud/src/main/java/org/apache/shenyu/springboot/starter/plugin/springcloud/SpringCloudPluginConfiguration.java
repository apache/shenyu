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

import com.netflix.loadbalancer.IRule;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.springcloud.SpringCloudPlugin;
import org.apache.shenyu.plugin.springcloud.context.SpringCloudShenyuContextDecorator;
import org.apache.shenyu.plugin.springcloud.handler.SpringCloudPluginDataHandler;
import org.apache.shenyu.plugin.springcloud.loadbalance.LoadBalanceRule;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.cloud.client.loadbalancer.LoadBalancerClient;
import org.springframework.cloud.netflix.ribbon.RibbonClientSpecification;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Spring cloud plugin configuration.
 */
@Configuration
public class SpringCloudPluginConfiguration {

    /**
     * init springCloud plugin.
     *
     * @param loadBalancerClient the load balancer client
     * @return {@linkplain SpringCloudPlugin}
     */
    @Bean
    public ShenyuPlugin springCloudPlugin(final ObjectProvider<LoadBalancerClient> loadBalancerClient) {
        return new SpringCloudPlugin(loadBalancerClient.getIfAvailable());
    }

    /**
     * Spring cloud shenyu context decorator shenyu context decorator.
     *
     * @return the shenyu context decorator
     */
    @Bean
    public ShenyuContextDecorator springCloudShenyuContextDecorator() {
        return new SpringCloudShenyuContextDecorator();
    }

    /**
     * Spring cloud plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler springCloudPluginDataHandler() {
        return new SpringCloudPluginDataHandler();
    }

    /**
     * Custom ribbon IRule.
     *
     * @return ribbonClientSpecification ribbonClientSpecification
     */
    @Bean
    public RibbonClientSpecification ribbonClientSpecification() {
        Class[] classes = new Class[]{SpringCloudClientConfiguration.class};
        return new RibbonClientSpecification(String.join(".", Constants.DEFAULT, RibbonClientSpecification.class.getName()), classes);
    }

    class SpringCloudClientConfiguration {
        @Bean
        public IRule ribbonRule() {
            return new LoadBalanceRule();
        }
    }
}
