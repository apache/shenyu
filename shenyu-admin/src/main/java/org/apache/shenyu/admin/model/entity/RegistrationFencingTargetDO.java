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

/**
 * Persisted target and boot-session state for business registration fencing.
 */
public class RegistrationFencingTargetDO extends BaseDO {

    private String domainId;

    private String namespaceId;

    private String selectorId;

    private String pluginId;

    private String protocol;

    private String host;

    private Integer port;

    private String principalId;

    private Boolean enabled;

    private String state;

    private Long generation;

    private String ownerStartupId;

    private String sessionId;

    private Long lastSequence;

    private String lastPayload;

    private String lastResult;

    private String lastRequestId;

    private String lastRequestPayload;

    private String retiredStartups;

    public String getDomainId() {
        return domainId;
    }

    public void setDomainId(final String domainId) {
        this.domainId = domainId;
    }

    public String getNamespaceId() {
        return namespaceId;
    }

    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    public String getSelectorId() {
        return selectorId;
    }

    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
    }

    public String getPluginId() {
        return pluginId;
    }

    public void setPluginId(final String pluginId) {
        this.pluginId = pluginId;
    }

    public String getProtocol() {
        return protocol;
    }

    public void setProtocol(final String protocol) {
        this.protocol = protocol;
    }

    public String getHost() {
        return host;
    }

    public void setHost(final String host) {
        this.host = host;
    }

    public Integer getPort() {
        return port;
    }

    public void setPort(final Integer port) {
        this.port = port;
    }

    public String getPrincipalId() {
        return principalId;
    }

    public void setPrincipalId(final String principalId) {
        this.principalId = principalId;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    public String getState() {
        return state;
    }

    public void setState(final String state) {
        this.state = state;
    }

    public Long getGeneration() {
        return generation;
    }

    public void setGeneration(final Long generation) {
        this.generation = generation;
    }

    public String getOwnerStartupId() {
        return ownerStartupId;
    }

    public void setOwnerStartupId(final String ownerStartupId) {
        this.ownerStartupId = ownerStartupId;
    }

    public String getSessionId() {
        return sessionId;
    }

    public void setSessionId(final String sessionId) {
        this.sessionId = sessionId;
    }

    public Long getLastSequence() {
        return lastSequence;
    }

    public void setLastSequence(final Long lastSequence) {
        this.lastSequence = lastSequence;
    }

    public String getLastPayload() {
        return lastPayload;
    }

    public void setLastPayload(final String lastPayload) {
        this.lastPayload = lastPayload;
    }

    public String getLastResult() {
        return lastResult;
    }

    public void setLastResult(final String lastResult) {
        this.lastResult = lastResult;
    }

    public String getLastRequestId() {
        return lastRequestId;
    }

    public void setLastRequestId(final String lastRequestId) {
        this.lastRequestId = lastRequestId;
    }

    public String getLastRequestPayload() {
        return lastRequestPayload;
    }

    public void setLastRequestPayload(final String lastRequestPayload) {
        this.lastRequestPayload = lastRequestPayload;
    }

    public String getRetiredStartups() {
        return retiredStartups;
    }

    public void setRetiredStartups(final String retiredStartups) {
        this.retiredStartups = retiredStartups;
    }
}
