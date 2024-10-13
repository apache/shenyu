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

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import java.io.Serializable;
import java.util.Objects;

/**
 * DiscoveryVO.
 */
public class DiscoveryVO implements Serializable {

    private static final long serialVersionUID = 6688267939899717881L;

    private String id;

    /**
     * name.
     */
    @NotNull(message = "name not null")
    private String name;

    /**
     * discovery type.
     */
    @NotNull(message = "type not null")
    private String type;

    /**
     * handler.
     */
    @NotNull(message = "level not null")
    private String level;

    /**
     * serviceList.
     */
    @NotNull(message = "serverList not null")
    private String serverList;

    /**
     * listenerNode.
     */
    @NotNull(message = "pluginName not null")
    private String pluginName;

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
    private DiscoveryHandlerVO discoveryHandler;

    /**
     * discoveryRel.
     */
    private DiscoveryRelVO discoveryRel;

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
     * get handler.
     *
     * @return handler
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
     * get pluginName.
     *
     * @return pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * set pluginName.
     *
     * @param pluginName pluginName
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
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
     * Get discoveryHandler.
     * @return discoveryHandler
     */
    public DiscoveryHandlerVO getDiscoveryHandler() {
        return discoveryHandler;
    }

    /**
     * Set discoveryHandler.
     * @param discoveryHandler discoveryHandler
     */
    public void setDiscoveryHandler(final DiscoveryHandlerVO discoveryHandler) {
        this.discoveryHandler = discoveryHandler;
    }

    /**
     * Get discoveryRel.
     * @return discoveryRel
     */
    public DiscoveryRelVO getDiscoveryRel() {
        return discoveryRel;
    }

    /**
     * Set discoveryRel.
     * @param discoveryRel discoveryRel
     */
    public void setDiscoveryRel(final DiscoveryRelVO discoveryRel) {
        this.discoveryRel = discoveryRel;
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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        DiscoveryVO that = (DiscoveryVO) o;
        return Objects.equals(id, that.id) && Objects.equals(name, that.name) && Objects.equals(type, that.type)
                && Objects.equals(level, that.level) && Objects.equals(serverList, that.serverList)
                && Objects.equals(pluginName, that.pluginName) && Objects.equals(props, that.props)
                && Objects.equals(discoveryHandler, that.discoveryHandler)
                && Objects.equals(discoveryRel, that.discoveryRel)
                && Objects.equals(namespaceId, that.namespaceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, name, type, level, serverList, pluginName, props, discoveryHandler, discoveryRel, namespaceId);
    }
}
