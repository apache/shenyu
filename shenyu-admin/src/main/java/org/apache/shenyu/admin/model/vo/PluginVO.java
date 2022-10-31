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

import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;

/**
 * this is plugin view to web front.
 */
public class PluginVO implements Serializable {

    private static final long serialVersionUID = 7537793180460522887L;

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

    public PluginVO() {
    }

    public PluginVO(final String id,
                    final String role,
                    final String name,
                    final String config,
                    final Integer sort,
                    final Boolean enabled,
                    final String dateCreated,
                    final String dateUpdated) {
        this.id = id;
        this.role = role;
        this.name = name;
        this.config = config;
        this.sort = sort;
        this.enabled = enabled;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
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
     * build pluginVO.
     *
     * @param pluginDO {@linkplain PluginDO}
     * @return {@linkplain PluginVO}
     */
    public static PluginVO buildPluginVO(final PluginDO pluginDO) {
        return new PluginVO(pluginDO.getId(), pluginDO.getRole(), pluginDO.getName(),
                pluginDO.getConfig(), pluginDO.getSort(), pluginDO.getEnabled(),
                DateUtils.localDateTimeToString(pluginDO.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(pluginDO.getDateUpdated().toLocalDateTime()));
    }
}
