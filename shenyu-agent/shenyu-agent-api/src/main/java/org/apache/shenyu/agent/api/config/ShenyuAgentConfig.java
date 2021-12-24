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
import java.util.List;
import java.util.Map;

/**
 * The type Shenyu agent config.
 */
public final class ShenyuAgentConfig {
    
    private String applicationName = "shenyu-agent";
    
    private Map<String, List<String>> supports = new HashMap<>();
    
    private Map<String, Map<String, AgentPluginConfig>> plugins = new HashMap<>();
    
    /**
     * Instantiates a new Shenyu agent config.
     */
    public ShenyuAgentConfig() {
    }
    
    /**
     * Instantiates a new Shenyu agent config.
     *
     * @param applicationName the application name
     * @param supports the supports
     * @param plugins the plugins
     */
    public ShenyuAgentConfig(final String applicationName, final Map<String, List<String>> supports, 
                             final Map<String, Map<String, AgentPluginConfig>> plugins) {
        this.applicationName = applicationName;
        this.supports = supports;
        this.plugins = plugins;
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
     * Gets supports.
     *
     * @return the supports
     */
    public Map<String, List<String>> getSupports() {
        return supports;
    }
    
    /**
     * Sets supports.
     *
     * @param supports the supports
     * @return the supports
     */
    public ShenyuAgentConfig setSupports(final Map<String, List<String>> supports) {
        this.supports = supports;
        return this;
    }
    
    /**
     * Gets plugins.
     *
     * @return the plugins
     */
    public Map<String, Map<String, AgentPluginConfig>> getPlugins() {
        return plugins;
    }
    
    /**
     * Sets plugins.
     *
     * @param plugins the plugins
     * @return the plugins
     */
    public ShenyuAgentConfig setPlugins(final Map<String, Map<String, AgentPluginConfig>> plugins) {
        this.plugins = plugins;
        return this;
    }
}
