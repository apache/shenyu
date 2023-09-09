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

import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.Properties;

public class DiscoveryConfigRegisterDTO implements DataTypeParent {
    private String name;

    private String type;

    private String serverList;

    private Properties props;

    public DiscoveryConfigRegisterDTO() {
    }

    public DiscoveryConfigRegisterDTO(final String name, final String type, final String serverList, final Properties props) {
        this.name = name;
        this.type = type;
        this.serverList = serverList;
        this.props = props;
    }

    private DiscoveryConfigRegisterDTO(final Builder builder) {
        name = builder.name;
        type = builder.type;
        serverList = builder.serverList;
        props = builder.props;
    }

    @Override
    public DataType getType() {
        return DataType.DISCOVERY_CONFIG;
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
     * getName.
     *
     * @return String discovery names
     */
    public String getName() {
        return name;
    }

    /**
     * setName.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * setType.
     *
     * @param type type
     */
    public void setType(final String type) {
        this.type = type;
    }

    /**
     * getServerList.
     *
     * @return String serverList
     */
    public String getServerList() {
        return serverList;
    }

    /**
     * setServerList.
     *
     * @param serverList serverList
     */
    public void setServerList(final String serverList) {
        this.serverList = serverList;
    }

    /**
     * getProps.
     *
     * @return Properties props
     */
    public Properties getProps() {
        return props;
    }

    /**
     * setProps.
     *
     * @param props props
     */
    public void setProps(final Properties props) {
        this.props = props;
    }

    public static final class Builder {
        private String name;

        private String type;

        private String serverList;

        private Properties props;

        private Builder() {
        }

        /**
         * name.
         *
         * @param name name
         * @return Builder builder
         */
        public Builder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * type.
         *
         * @param type type
         * @return Builder builder
         */
        public Builder type(final String type) {
            this.type = type;
            return this;
        }

        /**
         * serverList.
         *
         * @param serverList serverList
         * @return Builder builder
         */
        public Builder serverList(final String serverList) {
            this.serverList = serverList;
            return this;
        }

        /**
         * props.
         *
         * @param props props
         * @return Builder builder
         */
        public Builder props(final Properties props) {
            this.props = props;
            return this;
        }

        /**
         * build.
         *
         * @return DiscoveryConfigRegisterDTO discovery config register dto
         */
        public DiscoveryConfigRegisterDTO build() {
            return new DiscoveryConfigRegisterDTO(this);
        }

    }
}
