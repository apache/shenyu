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

package org.apache.shenyu.admin.model.dto;

import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Objects;

/**
 * this is plugin from by web front.
 */
public class PluginDTO implements Serializable {
    
    private static final long serialVersionUID = 789913506331671329L;
    
    /**
     * primary key.
     */
    @Existed(provider = PluginMapper.class, nullOfIgnore = true, message = "the plugin is not exited")
    private String id;
    
    /**
     * plugin name.
     */
    @NotBlank
    private String name;
    
    /**
     * plugin config.
     */
    private String config;
    
    /**
     * plugin role.
     */
    @NotBlank
    private String role;
    
    /**
     * plugin sort.
     */
    @NotNull
    private Integer sort;
    
    /**
     * whether enabled.
     */
    @NotNull
    private Boolean enabled;
    
    /**
     * Gets the value of id.
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }
    
    /**
     * Sets the id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }
    
    /**
     * Gets the value of name.
     *
     * @return the value of name
     */
    public String getName() {
        return name;
    }
    
    /**
     * Sets the name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }
    
    /**
     * Gets the value of config.
     *
     * @return the value of config
     */
    public String getConfig() {
        return config;
    }
    
    /**
     * Sets the config.
     *
     * @param config config
     */
    public void setConfig(final String config) {
        this.config = config;
    }
    
    /**
     * Gets the value of role.
     *
     * @return the value of role
     */
    public String getRole() {
        return role;
    }
    
    /**
     * Sets the role.
     *
     * @param role role
     */
    public void setRole(final String role) {
        this.role = role;
    }
    
    /**
     * Gets the value of sort.
     *
     * @return the value of sort
     */
    public Integer getSort() {
        return sort;
    }
    
    /**
     * Sets the sort.
     *
     * @param sort sort
     */
    public void setSort(final Integer sort) {
        this.sort = sort;
    }
    
    /**
     * Gets the value of enabled.
     *
     * @return the value of enabled
     */
    public Boolean getEnabled() {
        return enabled;
    }
    
    /**
     * Sets the enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof PluginDTO)) {
            return false;
        }
        PluginDTO pluginDTO = (PluginDTO) o;
        return Objects.equals(id, pluginDTO.id)
                && Objects.equals(name, pluginDTO.name)
                && Objects.equals(config, pluginDTO.config)
                && Objects.equals(role, pluginDTO.role)
                && Objects.equals(sort, pluginDTO.sort)
                && Objects.equals(enabled, pluginDTO.enabled);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(id, name, config, role, sort, enabled);
    }
}
