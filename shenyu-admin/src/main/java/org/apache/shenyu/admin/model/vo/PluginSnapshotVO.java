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

/**
 * PluginSnapshotVO.
 */
public class PluginSnapshotVO {
    
    /**
     * plugin id.
     */
    private String id;
    
    /**
     * plugin name.
     */
    private String name;
    
    /**
     * plugin config.
     */
    private String config;
    
    /**
     * plugin rule.
     */
    private String role;
    
    /**
     * plugin handle count.
     */
    private Integer handleCount;
    
    /**
     * plugin selector count.
     */
    private Integer selectorCount;
    
    /**
     * get id.
     *
     * @return id
     */
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
     * get handle count.
     *
     * @return value
     */
    public Integer getHandleCount() {
        return handleCount;
    }
    
    /**
     * set handle count.
     *
     * @param handleCount value
     */
    public void setHandleCount(final Integer handleCount) {
        this.handleCount = handleCount;
    }
    
    /**
     * get selector count.
     *
     * @return value
     */
    public Integer getSelectorCount() {
        return selectorCount;
    }
    
    /**
     * set selector count.
     *
     * @param selectorCount value
     */
    public void setSelectorCount(final Integer selectorCount) {
        this.selectorCount = selectorCount;
    }
    
    @Override
    public String toString() {
        return "PluginSnapshotVO{"
                + "id='" + id + '\''
                + ", name='" + name + '\''
                + ", config='" + config + '\''
                + ", role='" + role + '\''
                + ", handleCount=" + handleCount
                + ", selectorCount=" + selectorCount
                + '}';
    }
}
