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

public class InstanceBeatInfoDTO {
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
     * server-issued lease identity presented on a renewal; null on a first
     * registration or from a legacy client. Unknown fields are ignored by Admins
     * without the lease capability, so this addition is wire-compatible with v1.
     */
    private String leaseId;

    /**
     * heartbeat interval the client would like to run at, in seconds; the server
     * decides the effective interval and returns it in the lease response. Null when
     * the client does not negotiate.
     */
    private Integer proposedIntervalSeconds;

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
     * get leaseId.
     *
     * @return leaseId
     */
    public String getLeaseId() {
        return leaseId;
    }

    /**
     * set leaseId.
     *
     * @param leaseId leaseId
     */
    public void setLeaseId(final String leaseId) {
        this.leaseId = leaseId;
    }

    /**
     * get proposedIntervalSeconds.
     *
     * @return proposedIntervalSeconds
     */
    public Integer getProposedIntervalSeconds() {
        return proposedIntervalSeconds;
    }

    /**
     * set proposedIntervalSeconds.
     *
     * @param proposedIntervalSeconds proposedIntervalSeconds
     */
    public void setProposedIntervalSeconds(final Integer proposedIntervalSeconds) {
        this.proposedIntervalSeconds = proposedIntervalSeconds;
    }

}
