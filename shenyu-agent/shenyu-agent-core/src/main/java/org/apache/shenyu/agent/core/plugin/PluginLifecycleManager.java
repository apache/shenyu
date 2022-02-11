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

package org.apache.shenyu.agent.core.plugin;

import org.apache.shenyu.agent.api.config.AgentPluginConfig;
import org.apache.shenyu.agent.api.spi.AgentPluginBootService;
import org.apache.shenyu.agent.core.loader.SPILoader;
import org.apache.shenyu.agent.core.utils.ShenyuAgentConfigUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * The type Plugin lifecycle manager.
 */
public class PluginLifecycleManager {
    
    private static final Logger LOG = LoggerFactory.getLogger(PluginLifecycleManager.class);
    
    /**
     * Startup.
     *
     * @param configMap the config map
     */
    public void startup(final Map<String, AgentPluginConfig> configMap) {
        Set<String> support = ShenyuAgentConfigUtils.getSupports();
        configMap.entrySet().stream()
                .filter(entry -> support.contains(entry.getKey()))
                .forEach(entry -> Optional.ofNullable(SPILoader.load(AgentPluginBootService.class, entry.getKey()))
                        .ifPresent(bootService -> {
                            try {
                                LOG.info("start shenyu plugin: {}", entry.getKey());
                                bootService.start(entry.getValue());
                                // CHECKSTYLE:OFF
                            } catch (final Throwable ex) {
                                // CHECKSTYLE:ON
                                LOG.error("Failed to start shenyu plugin", ex);
                            }
                        }));
    }
    
    /**
     * Close.
     */
    public void close() {
        SPILoader.loadList(AgentPluginBootService.class).forEach(each -> {
            try {
                each.close();
                // CHECKSTYLE:OFF
            } catch (final Throwable ex) {
                // CHECKSTYLE:ON
                LOG.error("Failed to close shenyu agent plugin", ex);
            }
        });
    }
}
