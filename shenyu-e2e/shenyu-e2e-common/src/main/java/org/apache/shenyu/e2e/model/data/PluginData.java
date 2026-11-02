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

package org.apache.shenyu.e2e.model.data;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.jetbrains.annotations.NotNull;

/**
 * Plugin Data.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class PluginData {
    
    /**
     * id.
     */
    private String id;
    
    /**
     * name.
     */
    @NotNull
    private String name;
    
    /**
     * config.
     */
    private String config;
    
    /**
     * role.
     */
    private String role;
    
    /**
     * enabled.
     */
    private Boolean enabled;

    private String namespaceId;

    /**
     * Get plugin name.
     * @return plugin name
     */
    public String getName() {
        return name;
    }

    /**
     * Set plugin name.
     * @param name plugin name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * Get plugin configuration.
     * @return plugin configuration
     */
    public String getConfig() {
        return config;
    }

    /**
     * Set plugin configuration.
     * @param config plugin configuration
     */
    public void setConfig(final String config) {
        this.config = config;
    }

    /**
     * Get enabled state.
     * @return enabled state
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * Set enabled state.
     * @param enabled enabled state
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Get namespace identifier.
     * @return namespace identifier
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * Set namespace identifier.
     * @param namespaceId namespace identifier
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }
    
}
