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

import javax.validation.constraints.NotNull;
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
    @NotNull(message = "handler not null")
    private String handler;

    /**
     * serviceList.
     */
    @NotNull(message = "serviceList not null")
    private String serviceList;

    /**
     * listenerNode.
     */
    @NotNull(message = "listenerNode not null")
    private String listenerNode;

    /**
     * props.
     */
    @NotNull(message = "props not null")
    private String props;

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
    public String getHandler() {
        return handler;
    }

    /**
     * set handler.
     *
     * @param handler handler
     */
    public void setHandler(final String handler) {
        this.handler = handler;
    }

    /**
     * get serviceList.
     *
     * @return serviceList
     */
    public String getServiceList() {
        return serviceList;
    }

    /**
     * set serviceList.
     *
     * @param serviceList serviceList
     */
    public void setServiceList(final String serviceList) {
        this.serviceList = serviceList;
    }

    /**
     * get listenerNode.
     *
     * @return listenerNode
     */
    public String getListenerNode() {
        return listenerNode;
    }

    /**
     * set listenerNode.
     *
     * @param listenerNode listenerNode
     */
    public void setListenerNode(final String listenerNode) {
        this.listenerNode = listenerNode;
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
                && Objects.equals(handler, that.handler) && Objects.equals(serviceList, that.serviceList)
                && Objects.equals(listenerNode, that.listenerNode) && Objects.equals(props, that.props);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, name, type, handler, serviceList, listenerNode, props);
    }
}
