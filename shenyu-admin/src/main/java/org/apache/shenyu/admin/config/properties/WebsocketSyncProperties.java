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

/**
 * the websocket sync strategy properties.
 */
@ConfigurationProperties(prefix = "shenyu.sync.websocket")
public class WebsocketSyncProperties {

    /**
     * default: true.
     */
    private boolean enabled = true;

    /**
     * default is 8192.
     */
    private int messageMaxSize;

    /**
     * allowOrigins.
     */
    private String allowOrigins;

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
     * get messageMaxSize.
     *
     * @return messageMaxSize
     */
    public int getMessageMaxSize() {
        return messageMaxSize;
    }

    /**
     * set messageMaxSize.
     *
     * @param messageMaxSize messageMaxSize
     */
    public void setMessageMaxSize(final int messageMaxSize) {
        this.messageMaxSize = messageMaxSize;
    }

    /**
     * set allowOrigins.
     * @return allowOrigins
     */
    public String getAllowOrigins() {
        return allowOrigins;
    }

    /**
     * get allowOrigins.
     * @param allowOrigins allowOrigins
     */
    public void setAllowOrigins(final String allowOrigins) {
        this.allowOrigins = allowOrigins;
    }
}
