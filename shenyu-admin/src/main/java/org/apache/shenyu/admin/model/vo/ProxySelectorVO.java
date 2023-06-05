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

import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;

/**
 * this is proxy selector view to web front.
 */
public class ProxySelectorVO implements Serializable {

    private static final long serialVersionUID = -1329374830009912963L;

    /**
     * id.
     */
    private String id;

    /**
     * proxy selector name.
     */
    private String name;

    /**
     * pluginName.
     */
    private String pluginName;

    /**
     * forward port.
     */
    private Integer forwardPort;

    /**
     * type.
     */
    private String type;

    /**
     * props.
     */
    private String props;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    /**
     * ProxySelectorVO.
     */
    public ProxySelectorVO() {

    }

    /**
     * ProxySelectorVO.
     *
     * @param id          id
     * @param name        name
     * @param pluginName  pluginName
     * @param forwardPort forwardPort
     * @param type        type
     * @param props       props
     * @param dateCreated dateCreated
     * @param dateUpdated dateUpdated
     */
    public ProxySelectorVO(final String id, final String name, final String pluginName, final Integer forwardPort,
                           final String type, final String props, final String dateCreated, final String dateUpdated) {

        this.id = id;
        this.name = name;
        this.pluginName = pluginName;
        this.forwardPort = forwardPort;
        this.type = type;
        this.props = props;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
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
     * getDateCreated.
     *
     * @return dateCreated
     */
    public String getDateCreated() {

        return dateCreated;
    }

    /**
     * setDateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final String dateCreated) {

        this.dateCreated = dateCreated;
    }

    /**
     * getDateUpdated.
     *
     * @return dateUpdated
     */
    public String getDateUpdated() {

        return dateUpdated;
    }

    /**
     * setDateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final String dateUpdated) {

        this.dateUpdated = dateUpdated;
    }

    /**
     * buildProxySelectorVO.
     *
     * @param proxySelectorDO proxySelectorDO
     * @return ProxySelectorVO
     */
    public static ProxySelectorVO buildProxySelectorVO(final ProxySelectorDO proxySelectorDO) {

        return new ProxySelectorVO(proxySelectorDO.getId(), proxySelectorDO.getName(), proxySelectorDO.getPluginName(),
                proxySelectorDO.getForwardPort(), proxySelectorDO.getType(), proxySelectorDO.getProps(),
                DateUtils.localDateTimeToString(proxySelectorDO.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(proxySelectorDO.getDateUpdated().toLocalDateTime()));
    }
}
