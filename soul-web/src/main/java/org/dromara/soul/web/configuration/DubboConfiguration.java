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
import org.dromara.soul.web.plugin.SoulPlugin;
import org.dromara.soul.web.plugin.dubbo.DubboProxyService;
import org.dromara.soul.web.plugin.function.DubboPlugin;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * the dubbo configuration.
 *
 * @author xiaoyu(Myth)
 */
@Configuration
public class DubboConfiguration {

    private final DubboProxyService dubboProxyService;

    private final LocalCacheManager localCacheManager;

    /**
     * Instantiates a new Dubbo configuration.
     *
     * @param dubboProxyService the dubbo proxy service
     * @param localCacheManager the local cache manager
     */
    @Autowired(required = false)
    public DubboConfiguration(final DubboProxyService dubboProxyService, final LocalCacheManager localCacheManager) {
        this.dubboProxyService = dubboProxyService;
        this.localCacheManager = localCacheManager;
    }

    /**
     * init dubboPlugin.
     *
     * @return {@linkplain DubboPlugin}
     */
    @Bean
    public SoulPlugin dubboPlugin() {
        return new DubboPlugin(localCacheManager, dubboProxyService);
    }

}

