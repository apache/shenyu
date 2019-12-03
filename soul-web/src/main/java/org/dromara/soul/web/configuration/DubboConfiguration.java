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

package org.dromara.soul.web.configuration;

import org.dromara.soul.web.cache.LocalCacheManager;
import org.dromara.soul.web.filter.DubboBodyWebFilter;
import org.dromara.soul.web.plugin.SoulPlugin;
import org.dromara.soul.web.plugin.after.DubboResponsePlugin;
import org.dromara.soul.web.plugin.dubbo.DefaultGenericParamResolveServiceImpl;
import org.dromara.soul.web.plugin.dubbo.DubboProxyService;
import org.dromara.soul.web.plugin.dubbo.GenericParamResolveService;
import org.dromara.soul.web.plugin.function.DubboPlugin;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.SearchStrategy;
import org.springframework.context.annotation.Bean;
import org.springframework.core.annotation.Order;
import org.springframework.web.server.WebFilter;

/**
 * the dubbo configuration.
 *
 * @author xiaoyu(Myth)
 */
public class DubboConfiguration {

    /**
     * Body web filter web filter.
     *
     * @return the web filter
     */
    @Bean
    @Order(20)
    public WebFilter dubboBodyWebFilter() {
        return new DubboBodyWebFilter();
    }

    /**
     * Generic param service generic param service.
     *
     * @return the generic param service
     */
    @Bean
    @ConditionalOnMissingBean(value = GenericParamResolveService.class, search = SearchStrategy.ALL)
    public GenericParamResolveService genericParamResolveService() {
        return new DefaultGenericParamResolveServiceImpl();
    }

    /**
     * init dubboPlugin.
     *
     * @param localCacheManager the local cache manager
     * @return {@linkplain DubboPlugin}
     */
    @Bean
    public SoulPlugin dubboPlugin(@Qualifier("localCacheManager") final LocalCacheManager localCacheManager) {
        return new DubboPlugin(localCacheManager, new DubboProxyService(genericParamResolveService()));
    }

    /**
     * Dubbo response plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin dubboResponsePlugin() {
        return new DubboResponsePlugin();
    }

}

