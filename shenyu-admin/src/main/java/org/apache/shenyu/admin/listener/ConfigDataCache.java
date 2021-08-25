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

package org.apache.shenyu.admin.listener;

/**
 * Data cache to compare if data has changed.
 *
 * @since 2.0.0
 */
public class ConfigDataCache {
    
    private final String group;

    private volatile int hashValue;

    private final String json;

    private volatile long lastModifyTime;
    
    /**
     * Instantiates a new Config data cache.
     *
     * @param group          the group
     * @param json           the json
     * @param hashValue      the hashValue
     * @param lastModifyTime the last modify time
     */
    public ConfigDataCache(final String group, final String json, final int hashValue, final long lastModifyTime) {
        this.group = group;
        this.json = json;
        this.hashValue = hashValue;
        this.lastModifyTime = lastModifyTime;
    }
    
    /**
     * Update.
     *
     * @param hashValue      the hashValue
     * @param lastModifyTime the last modify time
     */
    protected synchronized void update(final int hashValue, final long lastModifyTime) {
        this.hashValue = hashValue;
        this.lastModifyTime = lastModifyTime;
    }
    
    /**
     * Gets group.
     *
     * @return the group
     */
    public String getGroup() {
        return group;
    }
    
    /**
     * Gets hashValue.
     *
     * @return the hashValue
     */
    public int getHashValue() {
        return hashValue;
    }
    
    /**
     * Gets last modify time.
     *
     * @return the last modify time
     */
    public long getLastModifyTime() {
        return lastModifyTime;
    }
    
    /**
     * Gets json.
     *
     * @return the json
     */
    public String getJson() {
        return json;
    }

    @Override
    public String toString() {
        return "{"
                + "group='" + group + '\''
                + ", hashValue='" + hashValue + '\''
                + ", lastModifyTime=" + lastModifyTime
                + '}';
    }
}
