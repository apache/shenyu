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

package org.apache.shenyu.admin.config.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.List;

/**
 * Cluster properties.
 */
@ConfigurationProperties(prefix = "shenyu.cluster")
public class ClusterProperties {
    
    /**
     * Whether enabled cluster mode, default: false.
     */
    private boolean enabled = false;
    
    /**
     * the master select method type.
     */
    private String type;
    
    /**
     * cluster forward uri list.
     */
    private List<String> forwardList;
    
    /**
     * cluster select master task period.
     */
    private Long selectPeriod = 15L;
    
    /**
     * cluster master lock time to live.
     */
    private Long lockTtl = 15L;
    
    /**
     * Gets the value of enabled.
     *
     * @return the value of enabled
     */
    public boolean isEnabled() {
        return enabled;
    }
    
    /**
     * Sets the enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }
    
    /**
     * Get the select type.
     *
     * @return the select type
     */
    public String getType() {
        return type;
    }
    
    /**
     * Set the select type.
     *
     * @param type the select type
     */
    public void setType(final String type) {
        this.type = type;
    }
    
    /**
     * Gets the value of forwardList.
     *
     * @return the value of forwardList
     */
    public List<String> getForwardList() {
        return forwardList;
    }
    
    /**
     * Sets the forwardList.
     *
     * @param forwardList forwardList
     */
    public void setForwardList(final List<String> forwardList) {
        this.forwardList = forwardList;
    }
    
    /**
     * Gets the select master task period.
     * @return select master task period
     */
    public Long getSelectPeriod() {
        return selectPeriod;
    }
    
    /**
     * Sets select master task period.
     * @param selectPeriod select master task period
     */
    public void setSelectPeriod(final Long selectPeriod) {
        this.selectPeriod = selectPeriod;
    }
    
    /**
     * Gets the select master lock ttl.
     * @return lock ttl
     */
    public Long getLockTtl() {
        return lockTtl;
    }
    
    /**
     * Sets select master lock ttl.
     * @param lockTtl lock ttl
     */
    public void setLockTtl(final Long lockTtl) {
        this.lockTtl = lockTtl;
    }
}
