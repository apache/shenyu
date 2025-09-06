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

package org.apache.shenyu.admin.model.entity;

import java.sql.Timestamp;

/**
 * Registry do.
 */
public class RegistryDO extends BaseDO {

    /**
     * the model registryId.
     */
    private String registryId;

    /**
     * the model protocol.
     */
    private String protocol;

    /**
     * the model address.
     */
    private String address;

    /**
     * the model username.
     */
    private String username;

    /**
     * the model password.
     */
    private String password;

    /**
     * the model namespace.
     */
    private String namespace;

    /**
     * the model group.
     */
    private String registryGroup;

    /**
     * Gets the value of registryId.
     *
     * @return the value of registryId
     */
    public String getRegistryId() {
        return registryId;
    }

    /**
     * Sets the registryId.
     *
     * @param registryId registryId
     */
    public void setRegistryId(final String registryId) {
        this.registryId = registryId;
    }

    /**
     * Gets the value of protocol.
     *
     * @return the value of protocol
     */
    public String getProtocol() {
        return protocol;
    }

    /**
     * Sets the protocol.
     *
     * @param protocol protocol
     */
    public void setProtocol(final String protocol) {
        this.protocol = protocol;
    }

    /**
     * Gets the value of address.
     *
     * @return the value of address
     */
    public String getAddress() {
        return address;
    }

    /**
     * Sets the address.
     *
     * @param address address
     */
    public void setAddress(final String address) {
        this.address = address;
    }

    /**
     * Gets the value of username.
     *
     * @return the value of username
     */
    public String getUsername() {
        return username;
    }

    /**
     * Sets the username.
     *
     * @param username username
     */
    public void setUsername(final String username) {
        this.username = username;
    }

    /**
     * Gets the value of password.
     *
     * @return the value of password
     */
    public String getPassword() {
        return password;
    }

    /**
     * Sets the password.
     *
     * @param password password
     */
    public void setPassword(final String password) {
        this.password = password;
    }

    /**
     * Gets the value of namespace.
     *
     * @return the value of namespace
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * Sets the namespace.
     *
     * @param namespace namespace
     */
    public void setNamespace(final String namespace) {
        this.namespace = namespace;
    }

    /**
     * Gets the value of group.
     *
     * @return the value of group
     */
    public String getRegistryGroup() {
        return registryGroup;
    }

    /**
     * Sets the group.
     *
     * @param registryGroup registryGroup
     */
    public void setRegistryGroup(final String registryGroup) {
        this.registryGroup = registryGroup;
    }

    /**
     * builder.
     *
     * @return RegistryDOBuilder
     */
    public static RegistryDOBuilder builder() {
        return new RegistryDO.RegistryDOBuilder();
    }

    public static final class RegistryDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String registryId;

        private String protocol;

        private String address;

        private String username;

        private String password;

        private String namespace;

        private String registryGroup;

        private RegistryDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return RegistryDOBuilder.
         */
        public RegistryDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return RegistryDOBuilder.
         */
        public RegistryDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return RegistryDOBuilder.
         */
        public RegistryDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * registryId.
         *
         * @param registryId the registryId.
         * @return RegistryDOBuilder.
         */
        public RegistryDOBuilder registryId(final String registryId) {
            this.registryId = registryId;
            return this;
        }

        /**
         * protocol.
         *
         * @param protocol the protocol.
         * @return RegistryDOBuilder.
         */
        public RegistryDOBuilder protocol(final String protocol) {
            this.protocol = protocol;
            return this;
        }

        /**
         * address.
         *
         * @param address the address.
         * @return RegistryDOBuilder.
         */
        public RegistryDOBuilder address(final String address) {
            this.address = address;
            return this;
        }

        /**
         * username.
         *
         * @param username the username.
         * @return RegistryDOBuilder.
         */
        public RegistryDOBuilder username(final String username) {
            this.username = username;
            return this;
        }

        /**
         * password.
         *
         * @param password the password.
         * @return RegistryDOBuilder.
         */
        public RegistryDOBuilder password(final String password) {
            this.password = password;
            return this;
        }

        /**
         * namespace.
         *
         * @param namespace the namespace.
         * @return RegistryDOBuilder.
         */
        public RegistryDOBuilder namespace(final String namespace) {
            this.namespace = namespace;
            return this;
        }

        /**
         * group.
         *
         * @param group the group.
         * @return RegistryDOBuilder.
         */
        public RegistryDOBuilder registryGroup(final String group) {
            this.registryGroup = group;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public RegistryDO build() {
            RegistryDO registryDO = new RegistryDO();
            registryDO.setId(id);
            registryDO.setDateCreated(dateCreated);
            registryDO.setDateUpdated(dateUpdated);
            registryDO.setRegistryId(registryId);
            registryDO.setProtocol(protocol);
            registryDO.setAddress(address);
            registryDO.setUsername(username);
            registryDO.setPassword(password);
            registryDO.setNamespace(namespace);
            registryDO.setRegistryGroup(registryGroup);
            return registryDO;
        }
    }
}
