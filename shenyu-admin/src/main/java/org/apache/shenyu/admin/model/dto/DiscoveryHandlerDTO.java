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

public class DiscoveryHandlerDTO {

    private String id;

    private String discoveryId;

    private String handler;

    private String listenerNode;

    private String props;

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
     * getDiscoveryId.
     *
     * @return discoveryId
     */
    public String getDiscoveryId() {
        return discoveryId;
    }

    /**
     * setDiscoveryId.
     *
     * @param discoveryId discoveryId
     */
    public void setDiscoveryId(final String discoveryId) {
        this.discoveryId = discoveryId;
    }

    /**
     * getHandler.
     *
     * @return handler
     */
    public String getHandler() {
        return handler;
    }

    /**
     * setHandler.
     *
     * @param handler handler
     */
    public void setHandler(final String handler) {
        this.handler = handler;
    }

    /**
     * getListenerNode.
     *
     * @return listenerNode
     */
    public String getListenerNode() {
        return listenerNode;
    }

    /**
     * setListenerNode.
     *
     * @param listenerNode listenerNode
     */
    public void setListenerNode(final String listenerNode) {
        this.listenerNode = listenerNode;
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
}
