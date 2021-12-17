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

package org.apache.shenyu.plugin.grpc.resolver;

import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;
import java.util.Map;

/**
 * Shenyu Service instance.
 */
public class ShenyuServiceInstance {

    private String host;

    private int port;

    private Map<String, String> metadata;

    /**
     * Instantiates a new Shenyu service instance.
     */
    public ShenyuServiceInstance() {
    }

    /**
     * Instantiates a new Shenyu service instance.
     *
     * @param host     the host
     * @param port     the port
     * @param metadata the metadata
     */
    public ShenyuServiceInstance(final String host,
                                 final int port,
                                 final Map<String, String> metadata) {
        this.host = host;
        this.port = port;
        this.metadata = metadata;
    }

    /**
     * Instantiates a new Shenyu service instance.
     *
     * @param host the host
     * @param port the port
     */
    public ShenyuServiceInstance(final String host, final int port) {
        this(host, port, new HashMap<>());
    }

    /**
     * Gets host.
     *
     * @return the host
     */
    public String getHost() {
        return host;
    }

    /**
     * Sets host.
     *
     * @param host the host
     */
    public void setHost(final String host) {
        this.host = host;
    }

    /**
     * Gets port.
     *
     * @return the port
     */
    public int getPort() {
        return port;
    }

    /**
     * Sets port.
     *
     * @param port the port
     */
    public void setPort(final int port) {
        this.port = port;
    }

    /**
     * Gets metadata.
     *
     * @return the metadata
     */
    public Map<String, String> getMetadata() {
        return metadata;
    }

    /**
     * Sets metadata.
     *
     * @param metadata the metadata
     */
    public void setMetadata(final Map<String, String> metadata) {
        this.metadata = metadata;
    }

    /**
     * Get weight.
     *
     * @return int i
     */
    public int getWeight() {
        final String weightValue = metadata.get("weight");
        if (StringUtils.isEmpty(weightValue)) {
            return 0;
        }
        return Integer.parseInt(weightValue);
    }

    /**
     * Get status.
     *
     * @return String status
     */
    public String getStatus() {
        return metadata.get("status");
    }

    /**
     * Set weight.
     *
     * @param weight weight
     */
    public void setWeight(final int weight) {
        this.metadata.put("weight", String.valueOf(weight));
    }

    /**
     * Set status.
     *
     * @param status status
     */
    public void setStatus(final boolean status) {
        this.metadata.put("status", String.valueOf(status));
    }
}
