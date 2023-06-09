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

package org.apache.shenyu.register.instance.api.entity;

import java.util.Objects;

/**
 * The type Instance register dto.
 */
public class InstanceEntity {

    private String appName;

    private String host;

    private Integer port;
    
    /**
     * Instantiates a new Instance register dto.
     *
     * @param appName the app name
     * @param host the host
     * @param port the port
     */
    public InstanceEntity(final String appName, final String host, final Integer port) {
        this.appName = appName;
        this.host = host;
        this.port = port;
    }
    
    /**
     * Instantiates a new Instance register dto.
     */
    public InstanceEntity() {
    }

    private InstanceEntity(final Builder builder) {
        appName = builder.appName;
        host = builder.host;
        port = builder.port;
    }
    
    
    /**
     * return builder.
     *
     * @return Builder builder
     */
    public static Builder builder() {
        return new Builder();
    }
    
    /**
     * getAppName.
     *
     * @return String app name
     */
    public String getAppName() {
        return appName;
    }
    
    /**
     * setAppName.
     *
     * @param appName appName
     */
    public void setAppName(final String appName) {
        this.appName = appName;
    }
    
    /**
     * getHost.
     *
     * @return String host
     */
    public String getHost() {
        return host;
    }
    
    /**
     * setHost.
     *
     * @param host host
     */
    public void setHost(final String host) {
        this.host = host;
    }
    
    /**
     * getPort.
     *
     * @return String port
     */
    public Integer getPort() {
        return port;
    }
    
    /**
     * setPort.
     *
     * @param port port
     */
    public void setPort(final Integer port) {
        this.port = port;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return Boolean.TRUE;
        }

        if (o == null || getClass() != o.getClass()) {
            return Boolean.FALSE;
        }

        InstanceEntity that = (InstanceEntity) o;
        return Objects.equals(getAppName(), that.getAppName())
                && Objects.equals(getHost(), that.getHost())
                && Objects.equals(getPort(), that.getPort());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getAppName(), getHost(), getPort());
    }

    @Override
    public String toString() {
        return "URIRegisterDTO{"
                + "appName='"
                + appName
                + ", host='"
                + host
                + ", port="
                + port
                + '}';
    }
    
    /**
     * The type Builder.
     */
    public static final class Builder {

        private String appName;

        private String host;

        private Integer port;

        private Builder() {
        }
    
        /**
         * appName.
         *
         * @param appName appName
         * @return Builder builder
         */
        public Builder appName(final String appName) {
            this.appName = appName;
            return this;
        }
    
        /**
         * host.
         *
         * @param host host
         * @return Builder builder
         */
        public Builder host(final String host) {
            this.host = host;
            return this;
        }
    
        /**
         * port.
         *
         * @param port port
         * @return Builder builder
         */
        public Builder port(final Integer port) {
            this.port = port;
            return this;
        }
    
        /**
         * build.
         *
         * @return Builder instance register dto
         */
        public InstanceEntity build() {
            return new InstanceEntity(this);
        }
    }
}
