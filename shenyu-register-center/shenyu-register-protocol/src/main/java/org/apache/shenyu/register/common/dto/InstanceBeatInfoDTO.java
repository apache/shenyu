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

package org.apache.shenyu.register.common.dto;

import java.io.Serializable;
import java.util.Objects;

/**
 * InstanceBeatInfoDTO.
 */
public class InstanceBeatInfoDTO implements Serializable {

    private static final long serialVersionUID = 1L;

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
     * namespaceId.
     */
    private String namespaceId;

    /**
     * bootId (unique per-process execution UUID).
     */
    private String bootId;

    /**
     * readinessStatus (mapped from InstanceReadinessStatusEnum or InstanceStatusEnum name).
     */
    private String readinessStatus;

    /**
     * reasonCode (bounded string detailing readiness or failure reasons).
     */
    private String reasonCode;

    /**
     * observationTime (epoch timestamp of observation).
     */
    private Long observationTime;

    /**
     * Instantiates a new Instance beat info dto.
     */
    public InstanceBeatInfoDTO() {
    }

    /**
     * Instantiates a new Instance beat info dto.
     *
     * @param instanceIp   the instance ip
     * @param instancePort the instance port
     * @param instanceType the instance type
     * @param instanceInfo the instance info
     * @param namespaceId  the namespace id
     */
    public InstanceBeatInfoDTO(final String instanceIp, final String instancePort, final String instanceType,
                               final String instanceInfo, final String namespaceId) {
        this.instanceIp = instanceIp;
        this.instancePort = instancePort;
        this.instanceType = instanceType;
        this.instanceInfo = instanceInfo;
        this.namespaceId = namespaceId;
    }

    /**
     * Instantiates a new Instance beat info dto.
     *
     * @param instanceIp       the instance ip
     * @param instancePort     the instance port
     * @param instanceType     the instance type
     * @param instanceInfo     the instance info
     * @param namespaceId      the namespace id
     * @param bootId           the boot id
     * @param readinessStatus  the readiness status
     * @param reasonCode       the reason code
     * @param observationTime  the observation time
     */
    public InstanceBeatInfoDTO(final String instanceIp, final String instancePort, final String instanceType,
                               final String instanceInfo, final String namespaceId, final String bootId,
                               final String readinessStatus, final String reasonCode, final Long observationTime) {
        this.instanceIp = instanceIp;
        this.instancePort = instancePort;
        this.instanceType = instanceType;
        this.instanceInfo = instanceInfo;
        this.namespaceId = namespaceId;
        this.bootId = bootId;
        this.readinessStatus = readinessStatus;
        this.reasonCode = reasonCode;
        this.observationTime = observationTime;
    }

    private InstanceBeatInfoDTO(final Builder builder) {
        this.instanceIp = builder.instanceIp;
        this.instancePort = builder.instancePort;
        this.instanceType = builder.instanceType;
        this.instanceInfo = builder.instanceInfo;
        this.namespaceId = builder.namespaceId;
        this.bootId = builder.bootId;
        this.readinessStatus = builder.readinessStatus;
        this.reasonCode = builder.reasonCode;
        this.observationTime = builder.observationTime;
    }

    /**
     * class builder.
     *
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
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
     * get bootId.
     *
     * @return bootId
     */
    public String getBootId() {
        return bootId;
    }

    /**
     * set bootId.
     *
     * @param bootId bootId
     */
    public void setBootId(final String bootId) {
        this.bootId = bootId;
    }

    /**
     * get readinessStatus.
     *
     * @return readinessStatus
     */
    public String getReadinessStatus() {
        return readinessStatus;
    }

    /**
     * set readinessStatus.
     *
     * @param readinessStatus readinessStatus
     */
    public void setReadinessStatus(final String readinessStatus) {
        this.readinessStatus = readinessStatus;
    }

    /**
     * get reasonCode.
     *
     * @return reasonCode
     */
    public String getReasonCode() {
        return reasonCode;
    }

    /**
     * set reasonCode.
     *
     * @param reasonCode reasonCode
     */
    public void setReasonCode(final String reasonCode) {
        this.reasonCode = reasonCode;
    }

    /**
     * get observationTime.
     *
     * @return observationTime
     */
    public Long getObservationTime() {
        return observationTime;
    }

    /**
     * set observationTime.
     *
     * @param observationTime observationTime
     */
    public void setObservationTime(final Long observationTime) {
        this.observationTime = observationTime;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        InstanceBeatInfoDTO that = (InstanceBeatInfoDTO) o;
        return Objects.equals(instanceIp, that.instanceIp)
                && Objects.equals(instancePort, that.instancePort)
                && Objects.equals(instanceType, that.instanceType)
                && Objects.equals(instanceInfo, that.instanceInfo)
                && Objects.equals(namespaceId, that.namespaceId)
                && Objects.equals(bootId, that.bootId)
                && Objects.equals(readinessStatus, that.readinessStatus)
                && Objects.equals(reasonCode, that.reasonCode)
                && Objects.equals(observationTime, that.observationTime);
    }

    @Override
    public int hashCode() {
        return Objects.hash(instanceIp, instancePort, instanceType, instanceInfo, namespaceId,
                bootId, readinessStatus, reasonCode, observationTime);
    }

    @Override
    public String toString() {
        return "InstanceBeatInfoDTO{"
                + "instanceIp='" + instanceIp + '\''
                + ", instancePort='" + instancePort + '\''
                + ", instanceType='" + instanceType + '\''
                + ", instanceInfo='" + instanceInfo + '\''
                + ", namespaceId='" + namespaceId + '\''
                + ", bootId='" + bootId + '\''
                + ", readinessStatus='" + readinessStatus + '\''
                + ", reasonCode='" + reasonCode + '\''
                + ", observationTime=" + observationTime
                + '}';
    }

    /**
     * The type Builder.
     */
    public static final class Builder {

        private String instanceIp;

        private String instancePort;

        private String instanceType;

        private String instanceInfo;

        private String namespaceId;

        private String bootId;

        private String readinessStatus;

        private String reasonCode;

        private Long observationTime;

        private Builder() {
        }

        /**
         * instanceIp.
         *
         * @param instanceIp instanceIp
         * @return Builder
         */
        public Builder instanceIp(final String instanceIp) {
            this.instanceIp = instanceIp;
            return this;
        }

        /**
         * instancePort.
         *
         * @param instancePort instancePort
         * @return Builder
         */
        public Builder instancePort(final String instancePort) {
            this.instancePort = instancePort;
            return this;
        }

        /**
         * instanceType.
         *
         * @param instanceType instanceType
         * @return Builder
         */
        public Builder instanceType(final String instanceType) {
            this.instanceType = instanceType;
            return this;
        }

        /**
         * instanceInfo.
         *
         * @param instanceInfo instanceInfo
         * @return Builder
         */
        public Builder instanceInfo(final String instanceInfo) {
            this.instanceInfo = instanceInfo;
            return this;
        }

        /**
         * namespaceId.
         *
         * @param namespaceId namespaceId
         * @return Builder
         */
        public Builder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }

        /**
         * bootId.
         *
         * @param bootId bootId
         * @return Builder
         */
        public Builder bootId(final String bootId) {
            this.bootId = bootId;
            return this;
        }

        /**
         * readinessStatus.
         *
         * @param readinessStatus readinessStatus
         * @return Builder
         */
        public Builder readinessStatus(final String readinessStatus) {
            this.readinessStatus = readinessStatus;
            return this;
        }

        /**
         * reasonCode.
         *
         * @param reasonCode reasonCode
         * @return Builder
         */
        public Builder reasonCode(final String reasonCode) {
            this.reasonCode = reasonCode;
            return this;
        }

        /**
         * observationTime.
         *
         * @param observationTime observationTime
         * @return Builder
         */
        public Builder observationTime(final Long observationTime) {
            this.observationTime = observationTime;
            return this;
        }

        /**
         * build InstanceBeatInfoDTO.
         *
         * @return InstanceBeatInfoDTO
         */
        public InstanceBeatInfoDTO build() {
            return new InstanceBeatInfoDTO(this);
        }
    }
}
