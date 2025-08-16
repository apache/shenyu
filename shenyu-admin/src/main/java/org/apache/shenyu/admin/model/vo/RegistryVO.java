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

package org.apache.shenyu.admin.model.vo;

import java.io.Serializable;

/**
 * RegistryVO.
 */
public class RegistryVO implements Serializable {

    private static final long serialVersionUID = 8274916350247389561L;

    /**
     * id.
     */
    private String id;

    /**
     * registryId.
     */
    private String registryId;

    /**
     * protocol.
     */
    private String protocol;

    /**
     * address.
     */
    private String address;

    /**
     * username.
     */
    private String username;

    /**
     * password.
     */
    private String password;

    /**
     * namespace.
     */
    private String namespace;

    /**
     * group.
     */
    private String group;

    /**
     * Gets the value of id.
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

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
    public String getGroup() {
        return group;
    }

    /**
     * Sets the group.
     *
     * @param group group
     */
    public void setGroup(final String group) {
        this.group = group;
    }
}
