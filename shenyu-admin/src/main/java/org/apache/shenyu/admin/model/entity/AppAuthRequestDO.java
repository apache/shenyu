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

import java.io.Serializable;
import java.util.Objects;
import java.util.Optional;

/**
 * App auth request idempotency record.
 * Unique key callerKey+namespaceId+requestId; state is CREATED/REVOKED; no credential copies are stored.
 */
public final class AppAuthRequestDO extends BaseDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Service identity digest (derived from the immutable userId, unaffected by user renaming).
     */
    private String callerKey;

    /**
     * namespace.
     */
    private String namespaceId;

    /**
     * Caller-generated idempotent request id.
     */
    private String requestId;

    /**
     * Canonical request digest (SHA-256), used to detect same-key different-parameter conflicts.
     */
    private String requestHash;

    /**
     * Associated app_auth id once created (the historical value is kept after REVOKED).
     */
    private String authId;

    /**
     * CREATED / REVOKED.
     */
    private String state;

    /**
     * Revocation reason (manual or caller provided).
     */
    private String revokedReason;

    /**
     * Gets the value of callerKey.
     *
     * @return the value of callerKey
     */
    public String getCallerKey() {
        return callerKey;
    }

    /**
     * Sets the callerKey.
     *
     * @param callerKey callerKey
     */
    public void setCallerKey(final String callerKey) {
        this.callerKey = callerKey;
    }

    /**
     * Gets the value of namespaceId.
     *
     * @return the value of namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * Sets the namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    /**
     * Gets the value of requestId.
     *
     * @return the value of requestId
     */
    public String getRequestId() {
        return requestId;
    }

    /**
     * Sets the requestId.
     *
     * @param requestId requestId
     */
    public void setRequestId(final String requestId) {
        this.requestId = requestId;
    }

    /**
     * Gets the value of requestHash.
     *
     * @return the value of requestHash
     */
    public String getRequestHash() {
        return requestHash;
    }

    /**
     * Sets the requestHash.
     *
     * @param requestHash requestHash
     */
    public void setRequestHash(final String requestHash) {
        this.requestHash = requestHash;
    }

    /**
     * Gets the value of authId.
     *
     * @return the value of authId
     */
    public String getAuthId() {
        return authId;
    }

    /**
     * Sets the authId.
     *
     * @param authId authId
     */
    public void setAuthId(final String authId) {
        this.authId = authId;
    }

    /**
     * Gets the value of state.
     *
     * @return the value of state
     */
    public String getState() {
        return state;
    }

    /**
     * Sets the state.
     *
     * @param state state
     */
    public void setState(final String state) {
        this.state = state;
    }

    /**
     * Gets the value of revokedReason.
     *
     * @return the value of revokedReason
     */
    public String getRevokedReason() {
        return revokedReason;
    }

    /**
     * Sets the revokedReason.
     *
     * @param revokedReason revokedReason
     */
    public void setRevokedReason(final String revokedReason) {
        this.revokedReason = revokedReason;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        final AppAuthRequestDO that = (AppAuthRequestDO) o;
        return Objects.equals(getId(), that.getId());
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(Optional.ofNullable(getId()).orElse(""));
    }

    @Override
    public String toString() {
        return "AppAuthRequestDO{id='" + getId() + "', callerKey='" + callerKey
                + "', namespaceId='" + namespaceId + "', requestId='" + requestId
                + "', state='" + state + "', authId='" + authId + "'}";
    }
}
