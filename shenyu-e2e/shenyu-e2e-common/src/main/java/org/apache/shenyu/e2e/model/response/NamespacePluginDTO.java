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

package org.apache.shenyu.e2e.model.response;

import com.fasterxml.jackson.annotation.JsonFormat;
import org.apache.commons.lang3.StringUtils;

import java.util.Date;

/**
 * this is namespace plugin from by web front.
 */
public class NamespacePluginDTO implements ResourceDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * primary key.
     */
    private String pluginId;


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
     * namespace id.
     */
    private String namespaceId;

    @JsonFormat(pattern = "YYYY-MM-dd HH:mm:ss")
    private Date dateCreated;

    @JsonFormat(pattern = "YYYY-MM-dd HH:mm:ss")
    private Date dateUpdated;


    /**
     * Gets the value of id.
     *
     * @return the value of id
     */
    @Override
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
    @Override
    public String getName() {
        return name;
    }

    /**
     * get dateCreated.
     *
     * @return dateCreated
     */
    @Override
    public Date getDateCreated() {
        return dateCreated;
    }

    /**
     * set dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * get dateUpdated.
     *
     * @return dateUpdated
     */
    @Override
    public Date getDateUpdated() {
        return dateUpdated;
    }

    /**
     * set dateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
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
        if (StringUtils.isBlank(config)) {
            return;
        }
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
     * Gets the namespace id.
     *
     * @return the namespace id
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespace Id.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }


    /**
     * Gets the plugin id.
     *
     * @return the plugin id
     */
    public String getPluginId() {
        return pluginId;
    }

    /**
     * set plugin id.
     *
     * @param pluginId pluginId
     */
    public void setPluginId(final String pluginId) {
        this.pluginId = pluginId;
    }
}
