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

package org.dromara.soul.extend.demo.config;

import org.dromara.soul.extend.demo.custom.CustomPlugin;
import org.dromara.soul.extend.demo.dubbo.CustomGenericParamServiceImpl;
import org.dromara.soul.web.cache.ZookeeperCacheManager;
import org.dromara.soul.web.plugin.SoulPlugin;
import org.dromara.soul.web.plugin.dubbo.GenericParamService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * CustomConfiguration.
 *
 * @author xiaoyu(Myth)
 */
@Configuration
public class CustomConfiguration {

    private final ZookeeperCacheManager zookeeperCacheManager;

    /**
     * Instantiates a new Custom configuration.
     *
     * @param zookeeperCacheManager the zookeeper cache manager
     */
    @Autowired(required = false)
    public CustomConfiguration(final ZookeeperCacheManager zookeeperCacheManager) {
        this.zookeeperCacheManager = zookeeperCacheManager;
    }

    /**
     * init Custom function plugin.
     *
     * @return SoulPlugin. soul plugin
     */
    @Bean
    public SoulPlugin functionPlugin() {
        return new CustomPlugin(zookeeperCacheManager);
    }

    /**
     * Generic param service generic param service.
     *
     * @return the generic param service
     */
    @Bean
    public GenericParamService genericParamService() {
        return new CustomGenericParamServiceImpl();
    }


}
