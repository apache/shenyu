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

package org.apache.shenyu.admin.model.event.instance;

/**
 * InstanceInfoReportDTO.
 */
public class InstanceInfoReportEvent {

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
     * namespace id.
     */
    private String namespaceId;

    public InstanceInfoReportEvent(final String instanceIp, final String instancePort, final String instanceType, final String instanceInfo, final String namespaceId) {
        this.instanceIp = instanceIp;
        this.instancePort = instancePort;
        this.instanceType = instanceType;
        this.instanceInfo = instanceInfo;
        this.namespaceId = namespaceId;
    }

    private InstanceInfoReportEvent(final Builder builder) {
        this.instanceIp = builder.instanceIp;
        this.instancePort = builder.instancePort;
        this.instanceType = builder.instanceType;
        this.instanceInfo = builder.instanceInfo;
        this.instanceState = builder.instanceSate;
        this.namespaceId = builder.namespaceId;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * get instance ip.
     *
     * @return instance ip
     */
    public String getInstanceIp() {
        return instanceIp;
    }

    /**
     * set instance ip.
     *
     * @param instanceIp instance ip
     */
    public void setInstanceIp(final String instanceIp) {
        this.instanceIp = instanceIp;
    }

    /**
     * get instance port.
     *
     * @return instance port
     */
    public String getInstancePort() {
        return instancePort;
    }

    /**
     * set instance port.
     *
     * @param instancePort instance port
     */
    public void setInstancePort(final String instancePort) {
        this.instancePort = instancePort;
    }

    /**
     * get instance type.
     *
     * @return instance type
     */
    public String getInstanceType() {
        return instanceType;
    }

    /**
     * set instance type.
     *
     * @param instanceType instance type
     */
    public void setInstanceType(final String instanceType) {
        this.instanceType = instanceType;
    }

    /**
     * get instance info.
     *
     * @return instance info
     */
    public String getInstanceInfo() {
        return instanceInfo;
    }

    /**
     * set instance info.
     *
     * @param instanceInfo instance info
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
     * get namespace id.
     *
     * @return namespace id
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespace id.
     *
     * @param namespaceId namespace id
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }


    public static final class Builder {

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
        private Integer instanceSate;

        /**
         * namespace id.
         */
        private String namespaceId;

        private Builder() {
        }

        /**
         * instance type.
         *
         * @param instanceType instance type
         * @return InstanceInfoRegisterDTO.Builder
         */
        public Builder instanceType(final String instanceType) {
            this.instanceType = instanceType;
            return this;
        }

        /**
         * instance info.
         *
         * @param instanceInfo instance info
         * @return InstanceInfoRegisterDTO.Builder
         */
        public Builder instanceInfo(final String instanceInfo) {
            this.instanceInfo = instanceInfo;
            return this;
        }

        /**
         * instance ip.
         *
         * @param instanceIp instance ip
         * @return InstanceInfoRegisterDTO.Builder
         */
        public Builder instanceIp(final String instanceIp) {
            this.instanceIp = instanceIp;
            return this;
        }

        /**
         * instance port.
         *
         * @param instancePort instance port
         * @return InstanceInfoRegisterDTO.Builder
         */
        public Builder instancePort(final String instancePort) {
            this.instancePort = instancePort;
            return this;
        }

        /**
         * instance state.
         *
         * @param instanceState instance state
         * @return InstanceInfoRegisterDTO.Builder
         */
        public Builder instanceState(final Integer instanceState) {
            this.instanceSate = instanceState;
            return this;
        }

        /**
         * namespace id.
         *
         * @param namespaceId namespace id
         * @return InstanceInfoRegisterDTO.Builder
         */
        public Builder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }

        /**
         * build.
         *
         * @return InstanceInfoRegisterDTO instance info register dto
         */
        public InstanceInfoReportEvent build() {
            return new InstanceInfoReportEvent(this);
        }

    }
}
