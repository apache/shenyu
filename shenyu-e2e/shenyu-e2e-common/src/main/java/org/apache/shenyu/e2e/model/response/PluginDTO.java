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

import java.util.Date;

/**
 * PluginDTO.
 */
public class PluginDTO implements ResourceDTO {
    
    private String id;

    private String name;

    private String role;

    private int sort;

    private boolean enabled;

    private String config;
    
    @JsonFormat(pattern = "YYYY-MM-dd HH:mm:ss")
    private Date dateCreated;

    @JsonFormat(pattern = "YYYY-MM-dd HH:mm:ss")
    private Date dateUpdated;

    /**
     * get id.
     *
     * @return id
     */
    @Override
    public String getId() {
        return id;
    }

    /**
     * set id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * get name.
     *
     * @return name
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get role.
     *
     * @return role
     */
    public String getRole() {
        return role;
    }

    /**
     * set role.
     *
     * @param role role
     */
    public void setRole(final String role) {
        this.role = role;
    }

    /**
     * get sort.
     *
     * @return sort
     */
    public int getSort() {
        return sort;
    }

    /**
     * set sort.
     *
     * @param sort sort
     */
    public void setSort(final int sort) {
        this.sort = sort;
    }

    /**
     * is enabled.
     *
     * @return enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * set enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * get config.
     *
     * @return config
     */
    public String getConfig() {
        return config;
    }

    /**
     * set config.
     *
     * @param config config
     */
    public void setConfig(final String config) {
        this.config = config;
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
}
