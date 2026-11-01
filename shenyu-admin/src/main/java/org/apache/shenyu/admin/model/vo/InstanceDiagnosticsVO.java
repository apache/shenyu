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

import java.io.Serializable;
import java.util.List;

/**
 * Instance diagnostics view: correlation metadata shared by status reports,
 * metric resources, logs and traces, plus resolved navigation links to the
 * operator-configured observability backends.
 */
public class InstanceDiagnosticsVO implements Serializable {

    private static final long serialVersionUID = 4728175609126640215L;

    /**
     * instance id.
     */
    private String instanceId;

    /**
     * stable instance identity, ip:port@type#namespace.
     */
    private String instanceKey;

    /**
     * namespace id.
     */
    private String namespaceId;

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
     * instance state.
     */
    private Integer instanceState;

    /**
     * last heartbeat time, epoch millis.
     */
    private Long lastHeartBeatTime;

    /**
     * boot identity: JVM start time in epoch millis, null for legacy instances
     * that do not report correlation fields.
     */
    private Long bootTime;

    /**
     * resolved time window start, epoch millis.
     */
    private Long startTime;

    /**
     * resolved time window end, epoch millis.
     */
    private Long endTime;

    /**
     * resolved observability links.
     */
    private List<ObservabilityLink> links;

    /**
     * get instanceId.
     *
     * @return instanceId
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
     * get instanceKey.
     *
     * @return instanceKey
     */
    public String getInstanceKey() {
        return instanceKey;
    }

    /**
     * set instanceKey.
     *
     * @param instanceKey instanceKey
     */
    public void setInstanceKey(final String instanceKey) {
        this.instanceKey = instanceKey;
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
     * get lastHeartBeatTime.
     *
     * @return lastHeartBeatTime
     */
    public Long getLastHeartBeatTime() {
        return lastHeartBeatTime;
    }

    /**
     * set lastHeartBeatTime.
     *
     * @param lastHeartBeatTime lastHeartBeatTime
     */
    public void setLastHeartBeatTime(final Long lastHeartBeatTime) {
        this.lastHeartBeatTime = lastHeartBeatTime;
    }

    /**
     * get bootTime.
     *
     * @return bootTime
     */
    public Long getBootTime() {
        return bootTime;
    }

    /**
     * set bootTime.
     *
     * @param bootTime bootTime
     */
    public void setBootTime(final Long bootTime) {
        this.bootTime = bootTime;
    }

    /**
     * get startTime.
     *
     * @return startTime
     */
    public Long getStartTime() {
        return startTime;
    }

    /**
     * set startTime.
     *
     * @param startTime startTime
     */
    public void setStartTime(final Long startTime) {
        this.startTime = startTime;
    }

    /**
     * get endTime.
     *
     * @return endTime
     */
    public Long getEndTime() {
        return endTime;
    }

    /**
     * set endTime.
     *
     * @param endTime endTime
     */
    public void setEndTime(final Long endTime) {
        this.endTime = endTime;
    }

    /**
     * get links.
     *
     * @return links
     */
    public List<ObservabilityLink> getLinks() {
        return links;
    }

    /**
     * set links.
     *
     * @param links links
     */
    public void setLinks(final List<ObservabilityLink> links) {
        this.links = links;
    }

    /**
     * A resolved navigation link to one observability backend.
     */
    public static class ObservabilityLink implements Serializable {

        /**
         * link type: metrics backend.
         */
        public static final String TYPE_METRICS = "METRICS";

        /**
         * link type: logging backend.
         */
        public static final String TYPE_LOGS = "LOGS";

        /**
         * link type: tracing backend.
         */
        public static final String TYPE_TRACE = "TRACE";

        /**
         * state: the link is resolved and ready for navigation.
         */
        public static final String STATE_READY = "READY";

        /**
         * state: no URL template is configured for this backend.
         */
        public static final String STATE_NOT_CONFIGURED = "NOT_CONFIGURED";

        /**
         * state: the instance does not report a correlation field required by the template.
         */
        public static final String STATE_MISSING_INSTANCE_DATA = "MISSING_INSTANCE_DATA";

        /**
         * state: the configured template is not a plain http/https URL or embeds credentials.
         */
        public static final String STATE_INVALID_TEMPLATE = "INVALID_TEMPLATE";

        private static final long serialVersionUID = -8154976209143372864L;

        /**
         * link type, one of METRICS / LOGS / TRACE.
         */
        private String type;

        /**
         * link state, one of the STATE_ constants.
         */
        private String state;

        /**
         * resolved url, only present when state is READY.
         */
        private String url;

        public ObservabilityLink() {
        }

        public ObservabilityLink(final String type, final String state, final String url) {
            this.type = type;
            this.state = state;
            this.url = url;
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
         * get state.
         *
         * @return state
         */
        public String getState() {
            return state;
        }

        /**
         * set state.
         *
         * @param state state
         */
        public void setState(final String state) {
            this.state = state;
        }

        /**
         * get url.
         *
         * @return url
         */
        public String getUrl() {
            return url;
        }

        /**
         * set url.
         *
         * @param url url
         */
        public void setUrl(final String url) {
            this.url = url;
        }
    }
}
