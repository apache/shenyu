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
 * The type Zookeeper configuration.
 */
@ConfigurationProperties(prefix = "shenyu.sync.zookeeper")
public class ZookeeperProperties {

    private String url;

    private Integer sessionTimeout;

    private Integer connectionTimeout;

    private String serializer;

    /**
     * Gets the value of url.
     *
     * @return the value of url
     */
    public String getUrl() {
        return url;
    }

    /**
     * Sets the url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * Gets the value of sessionTimeout.
     *
     * @return the value of sessionTimeout
     */
    public Integer getSessionTimeout() {
        return sessionTimeout;
    }

    /**
     * Sets the sessionTimeout.
     *
     * @param sessionTimeout sessionTimeout
     */
    public void setSessionTimeout(final Integer sessionTimeout) {
        this.sessionTimeout = sessionTimeout;
    }

    /**
     * Gets the value of connectionTimeout.
     *
     * @return the value of connectionTimeout
     */
    public Integer getConnectionTimeout() {
        return connectionTimeout;
    }

    /**
     * Sets the connectionTimeout.
     *
     * @param connectionTimeout connectionTimeout
     */
    public void setConnectionTimeout(final Integer connectionTimeout) {
        this.connectionTimeout = connectionTimeout;
    }

    /**
     * Gets the value of serializer.
     *
     * @return the value of serializer
     */
    public String getSerializer() {
        return serializer;
    }

    /**
     * Sets the serializer.
     *
     * @param serializer serializer
     */
    public void setSerializer(final String serializer) {
        this.serializer = serializer;
    }
}
