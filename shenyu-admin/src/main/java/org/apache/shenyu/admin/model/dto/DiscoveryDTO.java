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

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import java.io.Serializable;
import java.util.Objects;

/**
 * discovery bean form front.
 */
public class DiscoveryDTO implements Serializable {

    private static final long serialVersionUID = -5103326719088312738L;

    private String id;

    /**
     * plugin level.
     */
    @NotNull(message = "level not null")
    private String level;

    /**
     * name.
     */
    @NotNull(message = "name not null")
    private String name;

    /**
     * plugin name.
     */
    @NotNull(message = "pluginName not null")
    private String pluginName;

    /**
     * discovery type.
     */
    @NotNull(message = "type not null")
    private String type;

    /**
     * serviceList.
     */
    @NotNull(message = "serverList not null")
    private String serverList;

    /**
     * props.
     */
    @NotNull(message = "props not null")
    private String props;

    /**
     * namespaceId.
     */
    @NotBlank
    @Existed(message = "namespaceId is not existed", provider = NamespaceMapper.class)
    private String namespaceId;

    /**
     * discoveryHandler.
     */
    private DiscoveryHandlerDTO discoveryHandler;

    /**
     * discoveryRel.
     */
    private DiscoveryRelDTO discoveryRel;

    /**
     * get id.
     *
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * set id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * get name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get type.
     *
     * @return type
     */
    public String getType() {
        return type;
    }

    /**
     * set type.
     *
     * @param type type
     */
    public void setType(final String type) {
        this.type = type;
    }

    /**
     * get serverList.
     *
     * @return serverList
     */
    public String getServerList() {
        return serverList;
    }

    /**
     * set serverList.
     *
     * @param serverList serverList
     */
    public void setServerList(final String serverList) {
        this.serverList = serverList;
    }

    /**
     * get props.
     *
     * @return props
     */
    public String getProps() {
        return props;
    }

    /**
     * set props.
     *
     * @param props props
     */
    public void setProps(final String props) {
        this.props = props;
    }

    /**
     * get level.
     *
     * @return level
     */
    public String getLevel() {
        return level;
    }

    /**
     * set level.
     *
     * @param level level
     */
    public void setLevel(final String level) {
        this.level = level;
    }

    /**
     * get plugin name.
     *
     * @return plugin name
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * set plugin name.
     *
     * @param pluginName plugin name
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * Get discovery handler.
     * @return discovery handler
     */
    public DiscoveryHandlerDTO getDiscoveryHandler() {
        return discoveryHandler;
    }

    /**
     * Set discovery handler.
     * @param discoveryHandler discovery handler
     */
    public void setDiscoveryHandler(final DiscoveryHandlerDTO discoveryHandler) {
        this.discoveryHandler = discoveryHandler;
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

    /**
     * Get discovery rel.
     * @return discovery rel
     */
    public DiscoveryRelDTO getDiscoveryRel() {
        return discoveryRel;
    }

    /**
     * Set discovery rel.
     * @param discoveryRel discovery rel
     */
    public void setDiscoveryRel(final DiscoveryRelDTO discoveryRel) {
        this.discoveryRel = discoveryRel;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }

        DiscoveryDTO that = (DiscoveryDTO) o;
        return Objects.equals(id, that.id) && Objects.equals(name, that.name) && Objects.equals(type, that.type)
                && Objects.equals(serverList, that.serverList) && Objects.equals(props, that.props)
                && Objects.equals(level, that.level) && Objects.equals(pluginName, that.pluginName)
                && Objects.equals(discoveryHandler, that.discoveryHandler)
                && Objects.equals(discoveryRel, that.discoveryRel)
                && Objects.equals(namespaceId, that.namespaceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, name, type, serverList, props, level, pluginName, discoveryHandler, discoveryRel, namespaceId);
    }
}
