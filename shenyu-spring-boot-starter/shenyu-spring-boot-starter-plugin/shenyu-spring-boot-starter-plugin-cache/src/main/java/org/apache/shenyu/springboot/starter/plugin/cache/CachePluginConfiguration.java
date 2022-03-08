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

package org.apache.shenyu.springboot.starter.plugin.cache;

import org.apache.shenyu.plugin.cache.base.handler.CacheHandler;
import org.apache.shenyu.plugin.cache.read.CacheReadPlugin;
import org.apache.shenyu.plugin.cache.write.CacheWritePlugin;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * CachePluginConfiguration.
 */
@Configuration
public class CachePluginConfiguration {

    /**
     * the cache read plugin.
     * @return the shenyu plugin
     */
    @Bean
    public CacheReadPlugin cacheReadPlugin() {
        return new CacheReadPlugin();
    }

    /**
     * the cache write plugin.
     * @return the shenyu plugin
     */
    @Bean
    public CacheWritePlugin cacheWritePlugin() {
        return new CacheWritePlugin();
    }

    /**
     * Cache plugin data handler plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public CacheHandler cacheHandler() {
        return new CacheHandler();
    }
}
