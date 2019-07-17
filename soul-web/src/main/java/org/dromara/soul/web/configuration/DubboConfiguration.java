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
import org.dromara.soul.web.plugin.dubbo.GenericParamService;
import org.dromara.soul.web.plugin.dubbo.GenericParamServiceImpl;
import org.dromara.soul.web.plugin.function.DubboPlugin;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.SearchStrategy;
import org.springframework.context.annotation.Bean;

/**
 * the dubbo configuration.
 *
 * @author xiaoyu(Myth)
 */
public class DubboConfiguration {

    /**
     * Generic param service generic param service.
     *
     * @return the generic param service
     */
    @Bean
    @ConditionalOnMissingBean(value = GenericParamService.class, search = SearchStrategy.ALL)
    public GenericParamService genericParamService() {
        return new GenericParamServiceImpl();
    }

    /**
     * init dubboPlugin.
     *
     * @param localCacheManager the local cache manager
     * @return {@linkplain DubboPlugin}
     */
    @Bean
    public SoulPlugin dubboPlugin(LocalCacheManager localCacheManager) {
        return new DubboPlugin(localCacheManager, new DubboProxyService(genericParamService()));
    }

}

