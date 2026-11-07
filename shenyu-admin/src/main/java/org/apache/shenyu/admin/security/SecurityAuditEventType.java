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

package org.apache.shenyu.admin.security;

/**
 * Fixed set of security audit event types. The rate limiter allocates one bounded slot per
 * constant indexed by {@link Enum#ordinal()}, so event types must be declared here instead of
 * being invented at call sites.
 */
public enum SecurityAuditEventType {

    /**
     * dashboard username/password login attempts.
     */
    LOGIN("login"),

    /**
     * JWT verification outcomes at credential level.
     */
    TOKEN_VERIFICATION("token_verification"),

    /**
     * HTTP request level authentication outcomes in the stateless auth filter.
     */
    REQUEST_AUTH("request_auth"),

    /**
     * shiro permission check denials.
     */
    PERMISSION_CHECK("permission_check"),

    /**
     * app-auth caller, namespace and ownership access denials.
     */
    APP_AUTH_ACCESS("app_auth_access"),

    /**
     * app-auth credential lifecycle operations (apply / revoke / replacePaths).
     */
    APP_AUTH_LIFECYCLE("app_auth_lifecycle"),

    /**
     * cluster data-change-event shared-secret authentication.
     */
    CLUSTER_EVENT_AUTH("cluster_event_auth"),

    /**
     * websocket data-sync handshake shared-token authentication and origin checks.
     */
    WEBSOCKET_SYNC_AUTH("websocket_sync_auth"),

    /**
     * summary record for audit events suppressed by the rate limiter.
     */
    AUDIT_SUPPRESSED("audit_suppressed");

    private final String code;

    SecurityAuditEventType(final String code) {
        this.code = code;
    }

    /**
     * get the stable code written into audit log lines.
     *
     * @return event type code
     */
    public String getCode() {
        return code;
    }
}
