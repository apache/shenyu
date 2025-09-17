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

package org.apache.shenyu.infra.zookeeper.autoconfig;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.infra.common.InfraConstants;
import org.apache.shenyu.infra.common.InfraParentProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;

import static org.apache.shenyu.infra.zookeeper.autoconfig.ZookeeperProperties.CONFIG_PREFIX;

/**
 * The type Zookeeper configuration.
 */
@ConfigurationProperties(CONFIG_PREFIX)
public class ZookeeperProperties extends InfraParentProperties {

    public static final String CONFIG_PREFIX = PARENT_CONFIG_PREFIX + Constants.DOT + InfraConstants.SHENYU_ZOOKEEPER;

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
     * Get serializer.
     *
     * @return serializer
     */
    public String getSerializer() {
        return serializer;
    }

    /**
     * Set serializer.
     *
     * @param serializer serializer
     */
    public void setSerializer(final String serializer) {
        this.serializer = serializer;
    }

}
