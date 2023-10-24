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

package org.apache.shenyu.register.common.dto;

import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.Objects;

/**
 * The type URI register dto.
 */
public class URIRegisterDTO implements DataTypeParent {
    
    private String protocol;

    private String appName;

    private String contextPath;

    private String rpcType;

    private String host;

    private Integer port;

    private EventType eventType;

    
    /**
     * Instantiates a new Uri register dto.
     *
     * @param protocol the protocol
     * @param appName the app name
     * @param contextPath the context path
     * @param rpcType the rpc type
     * @param host the host
     * @param port the port
     * @param eventType the event type
     */
    public URIRegisterDTO(final String protocol, final String appName, final String contextPath,
                          final String rpcType, final String host, final Integer port,
                          final EventType eventType, final Integer prefixForwardEnable) {
        this.protocol = protocol;
        this.appName = appName;
        this.contextPath = contextPath;
        this.rpcType = rpcType;
        this.host = host;
        this.port = port;
        this.eventType = eventType;
    }
    
    /**
     * Instantiates a new Uri register dto.
     */
    public URIRegisterDTO() {
    }

    private URIRegisterDTO(final Builder builder) {
        protocol = builder.protocol;
        appName = builder.appName;
        contextPath = builder.contextPath;
        rpcType = builder.rpcType;
        host = builder.host;
        port = builder.port;
        eventType = builder.eventType;
    }
    
    /**
     * Trans form uri register dto.
     *
     * @param metaDataRegisterDTO the meta data register dto
     * @return the uri register dto
     */
    public static URIRegisterDTO transForm(final MetaDataRegisterDTO metaDataRegisterDTO) {
        return URIRegisterDTO.builder()
                .appName(metaDataRegisterDTO.getAppName())
                .contextPath(metaDataRegisterDTO.getContextPath())
                .rpcType(metaDataRegisterDTO.getRpcType())
                .host(metaDataRegisterDTO.getHost())
                .port(metaDataRegisterDTO.getPort()).build();
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
     * getType.
     *
     * @return String
     */
    @Override
    public DataType getType() {
        return DataType.URI;
    }
    
    /**
     * Gets protocol.
     *
     * @return the protocol
     */
    public String getProtocol() {
        return protocol;
    }
    
    /**
     * Sets protocol.
     *
     * @param protocol the protocol
     */
    public void setProtocol(final String protocol) {
        this.protocol = protocol;
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
     * getContextPath.
     *
     * @return String context path
     */
    public String getContextPath() {
        return contextPath;
    }
    
    /**
     * setContextPath.
     *
     * @param contextPath contextPath
     */
    public void setContextPath(final String contextPath) {
        this.contextPath = contextPath;
    }
    
    /**
     * getRpcType.
     *
     * @return String rpc type
     */
    public String getRpcType() {
        return rpcType;
    }
    
    /**
     * setRpcType.
     *
     * @param rpcType rpcType
     */
    public void setRpcType(final String rpcType) {
        this.rpcType = rpcType;
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
    
    /**
     * getEventType.
     *
     * @return String event type
     */
    public EventType getEventType() {
        return eventType;
    }
    
    /**
     * setEventType.
     *
     * @param eventType eventType
     */
    public void setEventType(final EventType eventType) {
        this.eventType = eventType;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return Boolean.TRUE;
        }

        if (o == null || getClass() != o.getClass()) {
            return Boolean.FALSE;
        }

        URIRegisterDTO that = (URIRegisterDTO) o;
        return Objects.equals(getAppName(), that.getAppName())
                && Objects.equals(getProtocol(), that.getProtocol())
                && Objects.equals(getContextPath(), that.getContextPath())
                && Objects.equals(getRpcType(), that.getRpcType())
                && Objects.equals(getHost(), that.getHost())
                && Objects.equals(getPort(), that.getPort())
                && getEventType() == that.getEventType();
    }

    @Override
    public int hashCode() {
        return Objects.hash(getProtocol(), getAppName(), getContextPath(), getRpcType(), getHost(), getPort(), getEventType());
    }

    @Override
    public String toString() {
        return "URIRegisterDTO{"
                + "protocol='"
                + protocol
                + '\''
                + ", appName='"
                + appName
                + '\''
                + ", contextPath='"
                + contextPath
                + '\''
                + ", rpcType='"
                + rpcType
                + '\''
                + ", host='"
                + host
                + '\''
                + ", port="
                + port
                + ", eventType="
                + eventType
                + '}';
    }

    /**
     * The type Builder.
     */
    public static final class Builder {
    
        private String protocol;

        private String appName;

        private String contextPath;

        private String rpcType;

        private String host;

        private Integer port;

        private EventType eventType;

        private Builder() {
        }
    
        /**
         * protocol.
         *
         * @param protocol protocol
         * @return Builder builder
         */
        public Builder protocol(final String protocol) {
            this.protocol = protocol;
            return this;
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
         * contextPath.
         *
         * @param contextPath contextPath
         * @return Builder builder
         */
        public Builder contextPath(final String contextPath) {
            this.contextPath = contextPath;
            return this;
        }
    
        /**
         * rpcType.
         *
         * @param rpcType rpcType
         * @return Builder builder
         */
        public Builder rpcType(final String rpcType) {
            this.rpcType = rpcType;
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
         * eventType.
         *
         * @param eventType eventType
         * @return Builder builder
         */
        public Builder eventType(final EventType eventType) {
            this.eventType = eventType;
            return this;
        }

        /**
         * build.
         *
         * @return Builder uri register dto
         */
        public URIRegisterDTO build() {
            return new URIRegisterDTO(this);
        }
    }
}
