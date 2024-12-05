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

import org.apache.shiro.codec.Base64;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

/**
 * this is plugin view to web front.
 */
public class NamespacePluginVO extends PluginVO implements Serializable {

    private static final long serialVersionUID = 4745279543203013538L;

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * namespace id.
     */
    private String namespaceId;

    /**
     * namespace id.
     */
    private String pluginRelId;

    /**
     * plugin jar byte.
     */
    private byte[] pluginJar;

    public NamespacePluginVO() {
    }

    public NamespacePluginVO(final String id, final String role, final String name, final String config, final Integer sort,
                             final Boolean enabled, final String dateCreated, final String dateUpdated, final String file,
                             final List<PluginHandleVO> pluginHandleList, final String pluginId, final String namespaceId,
                             final byte[] pluginJar) {
        super(id, role, name, config, sort, enabled, dateCreated, dateUpdated, file, pluginHandleList);
        this.pluginId = pluginId;
        this.namespaceId = namespaceId;
        this.pluginJar = pluginJar;
    }

    /**
     * Gets the namespace id.
     *
     * @return the plugin handle list
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespace id.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    /**
     * Gets the plugin jar.
     *
     * @return the plugin jar
     */
    public byte[] getPluginJar() {
        return pluginJar;
    }

    /**
     * set pluginJar.
     *
     * @param pluginJar pluginJar
     */
    public void setPluginJar(final byte[] pluginJar) {
        this.pluginJar = pluginJar;
    }

    /**
     * Gets the plugin id.
     *
     * @return the plugin id
     */
    public String getPluginId() {
        return pluginId;
    }

    /**
     * set pluginId.
     *
     * @param pluginId pluginId
     */
    public void setPluginId(final String pluginId) {
        this.pluginId = pluginId;
    }

    /**
     * Gets the plugin rel id.
     *
     * @return the plugin rel id
     */
    public String getPluginRelId() {
        return pluginRelId;
    }

    /**
     * set pluginRelId.
     *
     * @param pluginRelId pluginRelId
     */
    public void setPluginRelId(final String pluginRelId) {
        this.pluginRelId = pluginRelId;
    }

    /**
     * Gets the value of file.
     *
     * @return the value of file
     */
    @Override
    public String getFile() {
        return Optional.ofNullable(pluginJar).map(Base64::encodeToString).orElse("");
    }
}
