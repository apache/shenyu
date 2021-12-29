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

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * The type Shenyu agent config.
 */
public final class ShenyuAgentConfig {
    
    private String appName = "shenyu-agent";
    
    private Map<String, List<String>> supports = new LinkedHashMap<>();
    
    private Map<String, Map<String, AgentPluginConfig>> plugins = new LinkedHashMap<>();
    
    /**
     * Instantiates a new Shenyu agent config.
     */
    public ShenyuAgentConfig() {
    }
    
    /**
     * Instantiates a new Shenyu agent config.
     *
     * @param appName the application name
     * @param supports the supports
     * @param plugins the plugins
     */
    public ShenyuAgentConfig(final String appName, final Map<String, List<String>> supports, 
                             final Map<String, Map<String, AgentPluginConfig>> plugins) {
        this.appName = appName;
        this.supports = supports;
        this.plugins = plugins;
    }
    
    /**
     * Gets app name.
     *
     * @return the app name
     */
    public String getAppName() {
        return appName;
    }
    
    /**
     * Sets app name.
     *
     * @param appName the app name
     */
    public void setAppName(final String appName) {
        this.appName = appName;
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
     */
    public void setSupports(final Map<String, List<String>> supports) {
        this.supports = supports;
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
     */
    public void setPlugins(final Map<String, Map<String, AgentPluginConfig>> plugins) {
        this.plugins = plugins;
    }
}
