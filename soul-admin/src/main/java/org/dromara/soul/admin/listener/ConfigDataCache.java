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
 * @author huangxiaofeng
 * @date 2019/6/29 23:57
 * @since 2.0.0
 */
public class ConfigDataCache {

    protected final String group;

    private volatile String md5;

    private volatile long lastModifyTime;

    public ConfigDataCache(String group, String md5, long lastModifyTime) {
        this.group = group;
        this.md5 = md5;
        this.lastModifyTime = lastModifyTime;
    }

    protected synchronized void update(String md5, long lastModifyTime) {
        this.md5 = md5;
        this.lastModifyTime = lastModifyTime;
    }

    public String getGroup() {
        return group;
    }

    public String getMd5() {
        return md5;
    }

    public long getLastModifyTime() {
        return lastModifyTime;
    }
}
