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
 * DiscoveryHandlerDO.
 */
public class DiscoveryHandlerDO extends BaseDO {

    private String discoveryId;

    private String handler;

    private String listenerNode;

    private String props;

    public DiscoveryHandlerDO() {

    }

    public DiscoveryHandlerDO(final String discoveryId, final String handler, final String listenerNode, final String props) {
        this.discoveryId = discoveryId;
        this.handler = handler;
        this.listenerNode = listenerNode;
        this.props = props;
    }

    public DiscoveryHandlerDO(final String id, final Timestamp dateCreated, final Timestamp dateUpdated, final String discoveryId,
                              final String handler, final String listenerNode, final String props) {
        super(id, dateCreated, dateUpdated);
        this.discoveryId = discoveryId;
        this.handler = handler;
        this.listenerNode = listenerNode;
        this.props = props;
    }

    /**
     * get discovery id.
     *
     * @return discovery id
     */
    public String getDiscoveryId() {
        return discoveryId;
    }

    /**
     * set discovery id.
     *
     * @param discoveryId discovery id
     */
    public void setDiscoveryId(final String discoveryId) {
        this.discoveryId = discoveryId;
    }

    /**
     * get handler.
     *
     * @return the handler
     */
    public String getHandler() {
        return handler;
    }

    /**
     * set handler.
     *
     * @param handler the handler
     */
    public void setHandler(final String handler) {
        this.handler = handler;
    }

    /**
     * get listener node.
     *
     * @return the listener node
     */
    public String getListenerNode() {
        return listenerNode;
    }

    /**
     * set listener node.
     *
     * @param listenerNode the listener node
     */
    public void setListenerNode(final String listenerNode) {
        this.listenerNode = listenerNode;
    }

    /**
     * get props.
     *
     * @return the props
     */
    public String getProps() {
        return props;
    }

    /**
     * set props.
     *
     * @param props the props
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
        if (!super.equals(o)) {
            return false;
        }
        DiscoveryHandlerDO that = (DiscoveryHandlerDO) o;
        return Objects.equals(discoveryId, that.discoveryId)
                && Objects.equals(handler, that.handler)
                && Objects.equals(listenerNode, that.listenerNode)
                && Objects.equals(props, that.props);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), discoveryId, handler, listenerNode, props);
    }

    /**
     * builder.
     *
     * @return DiscoveryHandlerBuilder
     */
    public static DiscoveryHandlerDO.DiscoveryHandlerBuilder builder() {
        return new DiscoveryHandlerDO.DiscoveryHandlerBuilder();
    }

    public static final class DiscoveryHandlerBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String discoveryId;

        private String handler;

        private String listenerNode;

        private String props;

        private DiscoveryHandlerBuilder() {

        }

        /**
         * id.
         *
         * @param id the id.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryHandlerBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryHandlerBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryHandlerBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * discoveryId.
         *
         * @param discoveryId the discoveryId.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryHandlerBuilder discoveryId(final String discoveryId) {
            this.discoveryId = discoveryId;
            return this;
        }

        /**
         * handler.
         *
         * @param handler the handler.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryHandlerBuilder handler(final String handler) {
            this.handler = handler;
            return this;
        }

        /**
         * listenerNode.
         *
         * @param listenerNode the listenerNode.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryHandlerBuilder listenerNode(final String listenerNode) {
            this.listenerNode = listenerNode;
            return this;
        }

        /**
         * props.
         *
         * @param props the props.
         * @return DiscoveryRelDOBuilder.
         */
        public DiscoveryHandlerBuilder props(final String props) {
            this.props = props;
            return this;
        }

        /**
         * build DiscoveryHandlerDO.
         * @return DiscoveryHandlerDO
         */
        public DiscoveryHandlerDO build() {
            return new DiscoveryHandlerDO(id, dateCreated, dateUpdated, discoveryId, handler, listenerNode, props);
        }
    }
}
