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

package org.apache.shenyu.admin.model.dto;

import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * ProxySelectorDTO.
 */
public class ProxySelectorDTO implements Serializable {

    private static final long serialVersionUID = 3004856980753743799L;

    /**
     * id.
     */
    @Existed(provider = ProxySelectorMapper.class, nullOfIgnore = true, message = "proxy selector not exited")
    private String id;

    /**
     * proxy name.
     */
    @NotBlank
    private String name;

    /**
     * plugin name.
     */
    @NotBlank
    private String pluginName;

    /**
     * proxy type for tcp, upd, ws.
     */
    @NotBlank
    private String type;

    /**
     * proxy forward port.
     */
    @NotNull
    private Integer forwardPort;

    /**
     * other field.
     */
    @NotBlank
    private String props;

    /**
     * namespaceId.
     */
    @NotBlank
    @Existed(message = "namespaceId is not existed", provider = NamespaceMapper.class)
    private String namespaceId;

    /**
     * getId.
     *
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * setId.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * getName.
     *
     * @return name
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
     * getPluginName.
     *
     * @return pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * setPluginName.
     *
     * @param pluginName pluginName
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * getType.
     *
     * @return type
     */
    public String getType() {
        return type;
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
     * getForwardPort.
     *
     * @return forwardPort
     */
    public Integer getForwardPort() {
        return forwardPort;
    }

    /**
     * setForwardPort.
     *
     * @param forwardPort forwardPort
     */
    public void setForwardPort(final Integer forwardPort) {
        this.forwardPort = forwardPort;
    }

    /**
     * getProps.
     *
     * @return props
     */
    public String getProps() {
        return props;
    }

    /**
     * setProps.
     *
     * @param props props
     */
    public void setProps(final String props) {
        this.props = props;
    }

    /**
     * get namespaceId.
     *
     * @return namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }
}
