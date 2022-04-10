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
     * plugin handle count
     */
    private Integer handleCount;
    
    /**
     * plugin selector count
     */
    private Integer selectorCount;
    
    public String getId() {
        return id;
    }
    
    public void setId(String id) {
        this.id = id;
    }
    
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getConfig() {
        return config;
    }
    
    public void setConfig(String config) {
        this.config = config;
    }
    
    public String getRole() {
        return role;
    }
    
    public void setRole(String role) {
        this.role = role;
    }
    
    public Integer getHandleCount() {
        return handleCount;
    }
    
    public void setHandleCount(Integer handleCount) {
        this.handleCount = handleCount;
    }
    
    public Integer getSelectorCount() {
        return selectorCount;
    }
    
    public void setSelectorCount(Integer selectorCount) {
        this.selectorCount = selectorCount;
    }
    
    @Override
    public String toString() {
        return "PluginSnapshotVO{" +
                "id='" + id + '\'' +
                ", name='" + name + '\'' +
                ", config='" + config + '\'' +
                ", role='" + role + '\'' +
                ", handleCount=" + handleCount +
                ", selectorCount=" + selectorCount +
                '}';
    }
}
