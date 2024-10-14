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

import org.apache.shenyu.admin.model.dto.ProxySelectorAddDTO;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.util.StringUtils;

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * proxy selector do.
 */
public class ProxySelectorDO extends BaseDO {

    private static final long serialVersionUID = 6324671206584485506L;

    /**
     * proxy name.
     */
    private String name;

    /**
     * plugin name.
     */
    private String pluginName;

    /**
     * proxy type for tcp, upd, ws.
     */
    private String type;

    /**
     * proxy forward port.
     */
    private Integer forwardPort;

    /**
     * other field.
     */
    private String props;

    /**
     * namespaceId.
     */
    private String namespaceId;

    /**
     * builder.
     *
     * @return ProxySelectorBuilder
     */
    public static ProxySelectorBuilder builder() {

        return new ProxySelectorBuilder();
    }

    /**
     * buildProxySelectorDO.
     *
     * @param proxySelectorDTO proxySelectorDTO
     * @return ProxySelectorDO
     */
    public static ProxySelectorDO buildProxySelectorDO(final ProxySelectorAddDTO proxySelectorDTO) {

        return Optional.ofNullable(proxySelectorDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            ProxySelectorDO proxySelectorDO = ProxySelectorDO.builder()
                    .name(item.getName())
                    .pluginName(PluginEnum.TCP.getName())
                    .forwardPort(item.getForwardPort())
                    .type(item.getType())
                    .props(item.getProps())
                    .namespaceId(item.getNamespaceId())
                    .dateUpdated(currentTime).build();
            if (StringUtils.hasLength(item.getId())) {
                proxySelectorDO.setId(item.getId());
            } else {
                proxySelectorDO.setId(UUIDUtils.getInstance().generateShortUuid());
                proxySelectorDO.setDateCreated(currentTime);
            }
            return proxySelectorDO;
        }).orElse(null);
    }

    /**
     * buildProxySelectorDO.
     *
     * @param proxySelectorData proxySelectorData
     * @return ProxySelectorDO
     */
    public static ProxySelectorDO buildProxySelectorDO(final ProxySelectorData proxySelectorData) {

        return Optional.ofNullable(proxySelectorData).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            ProxySelectorDO proxySelectorDO = ProxySelectorDO.builder()
                    .name(item.getName())
                    .pluginName(PluginEnum.TCP.getName())
                    .forwardPort(item.getForwardPort())
                    .type(item.getType())
                    .props(JsonUtils.toJson(item.getProps()))
                    .dateUpdated(currentTime).build();
            if (StringUtils.hasLength(item.getId())) {
                proxySelectorDO.setId(item.getId());
            } else {
                proxySelectorDO.setId(UUIDUtils.getInstance().generateShortUuid());
                proxySelectorDO.setDateCreated(currentTime);
            }
            if (Objects.isNull(proxySelectorDO.getDateCreated())) {
                proxySelectorDO.setDateCreated(currentTime);
            }
            if (Objects.isNull(proxySelectorDO.getDateUpdated())) {
                proxySelectorDO.setDateUpdated(currentTime);
            }
            return proxySelectorDO;
        }).orElse(null);
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

    /**
     * ProxySelectorBuilder.
     */
    public static final class ProxySelectorBuilder {

        /**
         * id.
         */
        private String id;

        /**
         * dateCreated.
         */
        private Timestamp dateCreated;

        /**
         * dateUpdated.
         */
        private Timestamp dateUpdated;

        /**
         * name.
         */
        private String name;

        /**
         * pluginName.
         */
        private String pluginName;

        /**
         * type.
         */
        private String type;

        /**
         * forwardPort.
         */
        private Integer forwardPort;

        /**
         * props.
         */
        private String props;

        /**
         * namespaceId.
         */
        private String namespaceId;

        /**
         * ProxySelectorBuilder.
         */
        public ProxySelectorBuilder() {

        }

        /**
         * id.
         *
         * @param id the id.
         * @return ProxySelectorBuilder.
         */
        public ProxySelectorBuilder id(final String id) {

            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return ProxySelectorBuilder.
         */
        public ProxySelectorBuilder dateCreated(final Timestamp dateCreated) {

            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return ProxySelectorBuilder.
         */
        public ProxySelectorBuilder dateUpdated(final Timestamp dateUpdated) {

            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * name.
         *
         * @param name the name.
         * @return ProxySelectorBuilder.
         */
        public ProxySelectorBuilder name(final String name) {

            this.name = name;
            return this;
        }

        /**
         * pluginName.
         *
         * @param pluginName the pluginName.
         * @return ProxySelectorBuilder.
         */
        public ProxySelectorBuilder pluginName(final String pluginName) {

            this.pluginName = pluginName;
            return this;
        }

        /**
         * type.
         *
         * @param type the type.
         * @return ProxySelectorBuilder.
         */
        public ProxySelectorBuilder type(final String type) {

            this.type = type;
            return this;
        }

        /**
         * forwardPort.
         *
         * @param forwardPort the forwardPort.
         * @return ProxySelectorBuilder.
         */
        public ProxySelectorBuilder forwardPort(final Integer forwardPort) {

            this.forwardPort = forwardPort;
            return this;
        }

        /**
         * props.
         *
         * @param props other field.
         * @return ProxySelectorBuilder.
         */
        public ProxySelectorBuilder props(final String props) {

            this.props = props;
            return this;
        }

        /**
         * build namespaceId.
         *
         * @param namespaceId namespaceId
         * @return this
         */
        public ProxySelectorBuilder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }

        /**
         * build.
         *
         * @return ProxySelectorDO
         */
        public ProxySelectorDO build() {

            ProxySelectorDO proxySelectorDO = new ProxySelectorDO();
            proxySelectorDO.setId(this.id);
            proxySelectorDO.setName(this.name);
            proxySelectorDO.setPluginName(pluginName);
            proxySelectorDO.setForwardPort(this.forwardPort);
            proxySelectorDO.setType(this.type);
            proxySelectorDO.setProps(this.props);
            proxySelectorDO.setNamespaceId(this.namespaceId);
            proxySelectorDO.setDateCreated(this.dateCreated);
            proxySelectorDO.setDateUpdated(this.dateUpdated);
            return proxySelectorDO;
        }
    }
}
