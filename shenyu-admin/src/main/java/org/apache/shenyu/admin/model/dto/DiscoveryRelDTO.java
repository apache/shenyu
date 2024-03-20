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

import java.io.Serializable;

/**
 * DiscoveryRelDTO.
 */
public final class DiscoveryRelDTO implements Serializable {

    private static final long serialVersionUID = 7785307003003040858L;

    /**
     * id.
     */
    private String id;

    /**
     * pluginName.
     */
    private String pluginName;

    /**
     * discoveryHandlerId.
     */
    private String discoveryHandlerId;

    /**
     * selectorId.
     */
    private String selectorId;

    /**
     * proxySelectorId.
     */
    private String proxySelectorId;

    /**
     * Get id.
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * Set id.
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * get pluginName value.
     * @return pluginName value.
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * set pluginName value.
     * @param pluginName pluginName value.
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * get discovery handler id.
     * @return discovery handler id.
     */
    public String getDiscoveryHandlerId() {
        return discoveryHandlerId;
    }

    /**
     * set discovery handler id.
     * @param discoveryHandlerId discovery handler id.
     */
    public void setDiscoveryHandlerId(final String discoveryHandlerId) {
        this.discoveryHandlerId = discoveryHandlerId;
    }

    /**
     * get selector id.
     * @return selector id.
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * set selector id.
     * @param selectorId service id.
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
    }

    /**
     * get proxy selector id.
     * @return proxy selector id.
     */
    public String getProxySelectorId() {
        return proxySelectorId;
    }

    /**
     * set proxy selector id.
     * @param proxySelectorId proxy selector id
     */
    public void setProxySelectorId(final String proxySelectorId) {
        this.proxySelectorId = proxySelectorId;
    }

}
