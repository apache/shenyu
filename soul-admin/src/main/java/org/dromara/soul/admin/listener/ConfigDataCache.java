/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.admin.listener;

/**
 * Data cache to compare if data has changed.
 *
 * @author huangxiaofeng
 * @author xiaoyu
 * @since 2.0.0
 */
public class ConfigDataCache {

    /**
     * The Group.
     */
    protected final String group;

    private volatile String md5;

    private volatile long lastModifyTime;

    /**
     * Instantiates a new Config data cache.
     *
     * @param group          the group
     * @param md5            the md 5
     * @param lastModifyTime the last modify time
     */
    ConfigDataCache(String group, String md5, long lastModifyTime) {
        this.group = group;
        this.md5 = md5;
        this.lastModifyTime = lastModifyTime;
    }

    /**
     * Update.
     *
     * @param md5            the md 5
     * @param lastModifyTime the last modify time
     */
    protected synchronized void update(String md5, long lastModifyTime) {
        this.md5 = md5;
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
     * Gets md 5.
     *
     * @return the md 5
     */
    public String getMd5() {
        return md5;
    }

    /**
     * Gets last modify time.
     *
     * @return the last modify time
     */
    public long getLastModifyTime() {
        return lastModifyTime;
    }
}
