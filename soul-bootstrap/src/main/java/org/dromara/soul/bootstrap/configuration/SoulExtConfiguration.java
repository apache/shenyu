/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.bootstrap.configuration;

import org.dromara.soul.bootstrap.cors.CrossFilter;
import org.dromara.soul.bootstrap.dubbo.DubboMultiParameterResolveServiceImpl;
import org.dromara.soul.web.plugin.dubbo.GenericParamResolveService;
import org.dromara.soul.web.support.RemoteAddressResolver;
import org.dromara.soul.web.support.XForwardedRemoteAddressResolver;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.web.server.WebFilter;

/**
 * The type Soul ext configuration.
 *
 * @author xiaoyu(Myth)
 */
@Configuration
public class SoulExtConfiguration {
    
    /**
     * Cross filter web filter.
     * if you application has cross-domain.
     * this is demo.
     * 1. Customize webflux's cross-domain requests.
     * 2. Spring bean Sort is greater than -1.
     *
     * @return the web filter
     */
    @Bean
    @Order(-100)
    public WebFilter crossFilter() {
        return new CrossFilter();
    }
    
    /**
     * Remote address resolver remote address resolver.
     *
     * @return the remote address resolver
     */
    @Bean
    public RemoteAddressResolver remoteAddressResolver() {
        return new XForwardedRemoteAddressResolver(1);
    }
    
    /**
     * Generic param resolve service generic param resolve service.
     *
     * @return the generic param resolve service
     */
    @Bean
    @ConditionalOnProperty(name = "soul.dubbo.parameter", havingValue = "multi")
    public GenericParamResolveService genericParamResolveService() {
        return  new DubboMultiParameterResolveServiceImpl();
    }

   /* @Bean
    public SignService signService() {
        return (requestDTO, exchange) -> new Pair<>(true, "");
    }*/
}
