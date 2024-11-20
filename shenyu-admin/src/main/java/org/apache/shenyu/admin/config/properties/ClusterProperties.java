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

import com.google.common.collect.Lists;
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
    private boolean enabled;
    
    /**
     * the master select method type.
     */
    private String type;
    
    /**
     * the master schema (http/https).
     */
    private String schema = "http";
    
    /**
     * the connectionTimeout.
     */
    private int connectionTimeout = 6000;
    
    /**
     * the readTimeout.
     */
    private int readTimeout = 6000;
    
    /**
     * cluster forward uri list.
     */
    private List<String> forwardList = Lists.newArrayList();
    
    /**
     * cluster forward ignore uri list.
     */
    private List<String> ignoredList = Lists.newArrayList();
    
    /**
     * cluster select master task period.
     */
    private Long selectPeriod = 15L;
    
    /**
     * cluster master lock time to live.
     */
    private Long lockTtl = 30L;
    
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
     * Get the schema.
     *
     * @return schema
     */
    public String getSchema() {
        return schema;
    }
    
    /**
     * Set the schema.
     *
     * @param schema schema
     */
    public void setSchema(final String schema) {
        this.schema = schema;
    }
    
    /**
     * Get connection timeout.
     *
     * @return connectionTimeout
     */
    public int getConnectionTimeout() {
        return connectionTimeout;
    }
    
    /**
     * Set connection timeout.
     *
     * @param connectionTimeout connectionTimeout
     */
    public void setConnectionTimeout(final int connectionTimeout) {
        this.connectionTimeout = connectionTimeout;
    }
    
    /**
     * Get readTimeout.
     *
     * @return readTimeout
     */
    public int getReadTimeout() {
        return readTimeout;
    }
    
    /**
     * Set readTimeout.
     *
     * @param readTimeout readTimeout
     */
    public void setReadTimeout(final int readTimeout) {
        this.readTimeout = readTimeout;
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
     * Get the ignore list.
     *
     * @return the ignore list
     */
    public List<String> getIgnoredList() {
        return ignoredList;
    }
    
    /**
     * Set the ignore list.
     *
     * @param ignoredList the ignore list
     */
    public void setIgnoredList(final List<String> ignoredList) {
        this.ignoredList = ignoredList;
    }
    
    /**
     * Gets the select master task period.
     *
     * @return select master task period
     */
    public Long getSelectPeriod() {
        return selectPeriod;
    }
    
    /**
     * Sets select master task period.
     *
     * @param selectPeriod select master task period
     */
    public void setSelectPeriod(final Long selectPeriod) {
        this.selectPeriod = selectPeriod;
    }
    
    /**
     * Gets the select master lock ttl.
     *
     * @return lock ttl
     */
    public Long getLockTtl() {
        return lockTtl;
    }
    
    /**
     * Sets select master lock ttl.
     *
     * @param lockTtl lock ttl
     */
    public void setLockTtl(final Long lockTtl) {
        this.lockTtl = lockTtl;
    }
}
