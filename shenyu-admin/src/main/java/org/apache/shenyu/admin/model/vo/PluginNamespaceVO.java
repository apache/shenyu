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

package org.apache.shenyu.admin.model.vo;

import java.io.Serializable;
import java.util.List;

/**
 * this is plugin view to web front.
 */
public class PluginNamespaceVO implements Serializable {


    /**
     * primary key.
     */
    private String id;

    /**
     * plugin role.
     */
    private String role;

    /**
     * plugin name.
     */
    private String name;

    /**
     * plugin config.
     */
    private String config;

    /**
     * plugin sort.
     */
    private Integer sort;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    /**
     * file.
     */
    private String file;

    /**
     * namespace id.
     */
    private String namespaceId;

    /**
     * plugin handle List.
     */
    private List<PluginHandleVO> pluginHandleList;

    public PluginNamespaceVO() {
    }

    public PluginNamespaceVO(final String id,
                             final String role,
                             final String name,
                             final String config,
                             final Integer sort,
                             final Boolean enabled,
                             final String dateCreated,
                             final String dateUpdated,
                             final String file,
                             String namespaceId, final List<PluginHandleVO> pluginHandleList) {
        this.id = id;
        this.role = role;
        this.name = name;
        this.config = config;
        this.sort = sort;
        this.enabled = enabled;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
        this.file = file;
        this.namespaceId = namespaceId;
        this.pluginHandleList = pluginHandleList;
    }

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

    /**
     * Gets the value of dateCreated.
     *
     * @return the value of dateCreated
     */
    public String getDateCreated() {
        return dateCreated;
    }

    /**
     * Sets the dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final String dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * Gets the value of dateUpdated.
     *
     * @return the value of dateUpdated
     */
    public String getDateUpdated() {
        return dateUpdated;
    }

    /**
     * Sets the dateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final String dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    /**
     * Gets the value of plugin jar.
     *
     * @return the value of plugin
     */
    public String getFile() {
        return file;
    }

    /**
     * set plugin jar.
     *
     * @param file jar
     */
    public void setFile(final String file) {
        this.file = file;
    }

    /**
     * Gets the plugin handle list.
     *
     * @return the plugin handle list
     */
    public List<PluginHandleVO> getPluginHandleList() {
        return pluginHandleList;
    }

    /**
     * Sets the plugin handle list.
     *
     * @param pluginHandleList the plugin handle list
     */
    public void setPluginHandleList(final List<PluginHandleVO> pluginHandleList) {
        this.pluginHandleList = pluginHandleList;
    }

    /**
     * Gets the namespace Id.
     *
     * @return the plugin handle list
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespace Id.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(String namespaceId) {
        this.namespaceId = namespaceId;
    }
}
