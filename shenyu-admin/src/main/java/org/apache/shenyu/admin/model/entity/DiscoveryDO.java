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
import java.util.Objects;

/**
 * DiscoveryDO
 */
public final class DiscoveryDO extends BaseDO {

    private String name;

    private String type;

    private String handler;

    private String serviceList;

    private String listenerNode;

    private String props;

    public DiscoveryDO() {}

    public DiscoveryDO(String name, String type, String handler, String serviceList, String listenerNode, String props) {
        this.name = name;
        this.type = type;
        this.handler = handler;
        this.serviceList = serviceList;
        this.listenerNode = listenerNode;
        this.props = props;
    }

    public DiscoveryDO(String id, Timestamp dateCreated, Timestamp dateUpdated, String name, String type, String handler,
                       String serviceList, String listenerNode, String props) {
        super(id, dateCreated, dateUpdated);
        this.name = name;
        this.type = type;
        this.handler = handler;
        this.serviceList = serviceList;
        this.listenerNode = listenerNode;
        this.props = props;
    }

    /**
     * get the name value
     * @return the name value
     */
    public String getName() {
        return name;
    }

    /**
     * set the name value
     * @param name the name value
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * get the type value
     * @return the type value
     */
    public String getType() {
        return type;
    }

    /**
     * set the type value
     * @param type the type value
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * get the handler.
     * @return the handler.
     */
    public String getHandler() {
        return handler;
    }

    /**
     * set the handler.
     * @param handler the handler.
     */
    public void setHandler(String handler) {
        this.handler = handler;
    }

    /**
     * get the service list value
     * @return the service list value
     */
    public String getServiceList() {
        return serviceList;
    }

    /**
     * set the service list
     * @param serviceList he service list
     */
    public void setServiceList(String serviceList) {
        this.serviceList = serviceList;
    }

    /**
     * get list node value
     * @return list node value
     */
    public String getListenerNode() {
        return listenerNode;
    }

    /**
     * set list node value
     * @param listenerNode list node value
     */
    public void setListenerNode(String listenerNode) {
        this.listenerNode = listenerNode;
    }

    /**
     * get props value
     * @return props value
     */
    public String getProps() {
        return props;
    }

    /**
     * set props value
     * @param props props value
     */
    public void setProps(String props) {
        this.props = props;
    }

    public static DiscoveryDO.DiscoveryDOBuilder builder() {
        return new DiscoveryDO.DiscoveryDOBuilder();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        DiscoveryDO discoveryDO = (DiscoveryDO) o;
        return Objects.equals(name, discoveryDO.name)
                && Objects.equals(type, discoveryDO.type)
                && Objects.equals(handler, discoveryDO.handler)
                && Objects.equals(serviceList, discoveryDO.serviceList)
                && Objects.equals(listenerNode, discoveryDO.listenerNode)
                && Objects.equals(props, discoveryDO.props);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), name, type, handler, serviceList, listenerNode, props);
    }

    public static final class DiscoveryDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String name;

        private String type;

        private String handler;

        private String serviceList;

        private String listenerNode;

        private String props;

        private DiscoveryDOBuilder() {}

        /**
         * id.
         * @param id the id
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * name.
         *
         * @param name the type.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * type.
         *
         * @param type the type.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder type(final String type) {
            this.type = type;
            return this;
        }

        /**
         * handler.
         *
         * @param handler the handler.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder handler(final String handler) {
            this.handler = handler;
            return this;
        }

        /**
         * service list.
         *
         * @param serviceList the service list.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder serviceList(final String serviceList) {
            this.serviceList = serviceList;
            return this;
        }

        /**
         * listenerNode.
         *
         * @param listenerNode the listenerNode.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder listenerNode(final String listenerNode) {
            this.listenerNode = listenerNode;
            return this;
        }

        /**
         * props.
         *
         * @param props the props.
         * @return DiscoveryDOBuilder.
         */
        public DiscoveryDOBuilder props(final String props) {
            this.props = props;
            return this;
        }

        public DiscoveryDO build() {
            DiscoveryDO discoveryDO = new DiscoveryDO();
            discoveryDO.setId(id);
            discoveryDO.setDateCreated(dateCreated);
            discoveryDO.setDateUpdated(dateUpdated);
            discoveryDO.setName(name);
            discoveryDO.setType(type);
            discoveryDO.setHandler(handler);
            discoveryDO.setServiceList(serviceList);
            discoveryDO.setListenerNode(listenerNode);
            discoveryDO.setProps(props);
            return discoveryDO;
        }
    }
}
