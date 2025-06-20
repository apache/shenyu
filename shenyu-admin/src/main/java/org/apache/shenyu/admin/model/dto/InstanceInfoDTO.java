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
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import java.io.Serializable;
import java.util.Objects;

/**
 * InstanceInfoDTO.
 */
public class InstanceInfoDTO implements Serializable {

    private static final long serialVersionUID = 3644412439977354321L;

    /**
     * instance id.
     */
    private String instanceId;

    /**
     * instance ip.
     */
    private String instanceIp;

    /**
     * instance port.
     */
    private String instancePort;

    /**
     * instance type.
     */
    private String instanceType;

    /**
     * instance info.
     */
    private String instanceInfo;

    /**
     * instance state.
     */
    private Integer instanceState;

    /**
     * namespaceId.
     */
    @NotBlank
    @Existed(message = "namespaceId is not existed", provider = NamespaceMapper.class)
    private String namespaceId;

    /**
     * get getInstanceId.
     *
     * @return InstanceId
     */
    public String getInstanceId() {
        return instanceId;
    }

    /**
     * set instanceId.
     *
     * @param instanceId instanceId
     */
    public void setInstanceId(final String instanceId) {
        this.instanceId = instanceId;
    }

    /**
     * get instanceIp.
     *
     * @return instanceIp
     */
    public String getInstanceIp() {
        return instanceIp;
    }

    /**
     * set instanceIp.
     *
     * @param instanceIp instanceIp
     */
    public void setInstanceIp(final String instanceIp) {
        this.instanceIp = instanceIp;
    }

    /**
     * get instancePort.
     *
     * @return instancePort
     */
    public String getInstancePort() {
        return instancePort;
    }

    /**
     * set instancePort.
     *
     * @param instancePort instancePort
     */
    public void setInstancePort(final String instancePort) {
        this.instancePort = instancePort;
    }

    /**
     * get instanceType.
     *
     * @return instanceType
     */
    public String getInstanceType() {
        return instanceType;
    }

    /**
     * set instanceType.
     *
     * @param instanceType instanceType
     */
    public void setInstanceType(final String instanceType) {
        this.instanceType = instanceType;
    }

    /**
     * get instanceInfo.
     *
     * @return instanceInfo
     */
    public String getInstanceInfo() {
        return instanceInfo;
    }

    /**
     * set instanceInfo.
     *
     * @param instanceInfo instanceInfo
     */
    public void setInstanceInfo(final String instanceInfo) {
        this.instanceInfo = instanceInfo;
    }

    /**
     * get instanceState.
     *
     * @return instanceState
     */
    public Integer getInstanceState() {
        return instanceState;
    }

    /**
     * set instanceState.
     *
     * @param instanceState instanceState
     */
    public void setInstanceState(final Integer instanceState) {
        this.instanceState = instanceState;
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
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }

        InstanceInfoDTO that = (InstanceInfoDTO) o;
        return Objects.equals(instanceIp, that.instanceIp)
                && Objects.equals(instanceType, that.instanceType)
                && Objects.equals(instanceInfo, that.instanceInfo)
                && Objects.equals(namespaceId, that.namespaceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(instanceIp, instanceType, instanceInfo, namespaceId);
    }
}
