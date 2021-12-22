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

package org.apache.shenyu.agent.api.config;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * The type Shenyu agent config.
 */
public final class ShenyuAgentConfig {
    
    private String applicationName = "shenyu-agent";
    
    private Set<String> ignoredPluginNames = new HashSet<>();
    
    private Map<String, AgentPluginConfig> plugins = new HashMap<>();
    
    /**
     * Instantiates a new Shenyu agent config.
     */
    public ShenyuAgentConfig() {
    }
    
    /**
     * Instantiates a new Shenyu agent config.
     *
     * @param applicationName the application name
     * @param ignoredPluginNames the ignored plugin names
     * @param plugins the plugins
     */
    public ShenyuAgentConfig(final String applicationName, 
                             final Set<String> ignoredPluginNames, 
                             final Map<String, AgentPluginConfig> plugins) {
        this.applicationName = applicationName;
        this.ignoredPluginNames = ignoredPluginNames;
        this.plugins = plugins;
    }
    
    /**
     * Sets application name.
     *
     * @param applicationName the application name
     * @return the application name
     */
    public ShenyuAgentConfig setApplicationName(final String applicationName) {
        this.applicationName = applicationName;
        return this;
    }
    
    /**
     * Sets ignored plugin names.
     *
     * @param ignoredPluginNames the ignored plugin names
     * @return the ignored plugin names
     */
    public ShenyuAgentConfig setIgnoredPluginNames(final Set<String> ignoredPluginNames) {
        this.ignoredPluginNames = ignoredPluginNames;
        return this;
    }
    
    /**
     * Sets plugins.
     *
     * @param plugins the plugins
     * @return the plugins
     */
    public ShenyuAgentConfig setPlugins(final Map<String, AgentPluginConfig> plugins) {
        this.plugins = plugins;
        return this;
    }
    
    /**
     * Gets application name.
     *
     * @return the application name
     */
    public String getApplicationName() {
        return applicationName;
    }
    
    /**
     * Gets ignored plugin names.
     *
     * @return the ignored plugin names
     */
    public Set<String> getIgnoredPluginNames() {
        return ignoredPluginNames;
    }
    
    /**
     * Gets plugins.
     *
     * @return the plugins
     */
    public Map<String, AgentPluginConfig> getPlugins() {
        return plugins;
    }
}
