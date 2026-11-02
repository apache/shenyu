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

package org.apache.shenyu.register.common.protocol;

import org.apache.shenyu.register.common.dto.URIRegisterDTO;

/**
 * Wire contract for the opt-in business registration boot-session fencing API.
 *
 * <p>The legacy registration endpoints remain owned by {@link RegistrationProtocol}.
 * These endpoints accept only an opaque target and a validated session; a client
 * cannot select a different address through a mutation payload.</p>
 */
public final class RegistrationFencingProtocol {

    /** Capability advertised only by an explicitly enabled, fully guarded Admin cluster. */
    public static final String CAPABILITY = "boot-session-fencing-v1";

    /** Stable success code. */
    public static final String OK = "OK";

    /** Target configuration endpoint. */
    public static final String TARGETS_PATH = "/registration-fencing/targets";

    /** Target replacement endpoint. */
    public static final String REPLACE_PATH = TARGETS_PATH + "/{targetId}/replace";

    /** Target disable endpoint. */
    public static final String DISABLE_PATH = TARGETS_PATH + "/{targetId}/disable";

    /** Session acquisition endpoint. */
    public static final String ACQUIRE_PATH = "/shenyu-client/registration-sessions/acquire";

    /** Session mutation endpoint. */
    public static final String MUTATE_PATH = "/shenyu-client/registration-sessions/{sessionId}/mutate";

    /** Registration operation. */
    public static final String REGISTER = "REGISTER";

    /** Instance update operation. */
    public static final String UPDATE = "UPDATE";

    /** Heartbeat operation. */
    public static final String HEARTBEAT = "HEARTBEAT";

    /** Offline operation. */
    public static final String OFFLINE = "OFFLINE";

    /** Stable error codes returned by the fencing endpoints. */
    public static final String INVALID_REQUEST = "INVALID_REQUEST";

    public static final String FORBIDDEN = "FORBIDDEN";

    public static final String OWNER_CONFLICT = "OWNER_CONFLICT";

    public static final String CAS_MISMATCH = "CAS_MISMATCH";

    public static final String STALE_SESSION = "STALE_SESSION";

    public static final String SUPERSEDED = "SUPERSEDED";

    public static final String SESSION_RELEASED = "SESSION_RELEASED";

    public static final String FENCING_REQUIRED = "FENCING_REQUIRED";

    public static final String SOURCE_CONFLICT = "SOURCE_CONFLICT";

    public static final String SEQUENCE_CONFLICT = "SEQUENCE_CONFLICT";

    public static final String SEQUENCE_GAP = "SEQUENCE_GAP";

    public static final String OBSOLETE_OPERATION = "OBSOLETE_OPERATION";

    public static final String IDEMPOTENCY_CONFLICT = "IDEMPOTENCY_CONFLICT";

    public static final String TARGET_NOT_FOUND = "TARGET_NOT_FOUND";

    public static final String FENCING_NOT_ENABLED = "FENCING_NOT_ENABLED";

    private RegistrationFencingProtocol() {
    }

    /**
     * Target request.  Host and port are accepted only on this root-authenticated
     * pre-provisioning endpoint.
     *
     * @param domainId registration authority domain
     * @param namespaceId namespace identifier
     * @param selectorId existing selector identifier
     * @param protocol existing upstream protocol
     * @param host existing upstream host
     * @param port existing upstream port
     * @param principalId user id allowed to acquire the target
     */
    public record TargetRequest(String domainId, String namespaceId, String selectorId,
                                String protocol, String host, Integer port, String principalId) {
    }

    /**
     * Root CAS replacement request.
     *
     * @param requestId retry idempotency key
     * @param expectedStartupId current owner startup
     * @param expectedGeneration current generation
     * @param startupId replacement startup
     * @param principalId replacement business principal
     */
    public record ReplaceRequest(String requestId, String expectedStartupId, String expectedGeneration,
                                 String startupId, String principalId) {
    }

    /**
     * Business session acquisition request.
     *
     * @param domainId registration authority domain
     * @param targetId opaque pre-provisioned target
     * @param startupId process startup identifier
     * @param expectedGeneration expected generation
     */
    public record AcquireRequest(String domainId, String targetId, String startupId, String expectedGeneration) {
    }

    /**
     * Synchronous, sequenced business mutation request.
     *
     * @param domainId registration authority domain
     * @param sessionId session identifier
     * @param generation session generation
     * @param clientSequence per-session sequence
     * @param operation operation name
     * @param payload fixed-instance URI payload
     */
    public record MutationRequest(String domainId, String sessionId, String selectorId, String generation,
                                  String clientSequence, String operation, URIRegisterDTO payload) {
    }

    /**
     * Common response envelope.
     *
     * @param code stable response code
     * @param message diagnostic message without credentials
     * @param data response data
     * @param <T> data type
     */
    public record Response<T>(String code, String message, T data) {

        /**
         * Build a success response.
         *
         * @param data response data
         * @param <T> data type
         * @return success response
         */
        public static <T> Response<T> ok(final T data) {
            return new Response<>("OK", "OK", data);
        }

        /**
         * Build an error response.
         *
         * @param code stable error code
         * @param message safe message
         * @param <T> data type
         * @return error response
         */
        public static <T> Response<T> error(final String code, final String message) {
            return new Response<>(code, message, null);
        }
    }

    /**
     * Session response data.
     *
     * @param domainId registration authority domain
     * @param targetId opaque target
     * @param sessionId session identifier
     * @param startupId owner startup
     * @param generation generation
     * @param state session state
     * @param lastSequence last committed sequence
     * @param host canonical target host
     * @param port target port
     * @param protocol target protocol
     */
    public record SessionData(String domainId, String targetId, String namespaceId, String selectorId,
                              String sessionId, String startupId, String generation, String state, String lastSequence,
                              String host, Integer port, String protocol) {
    }

    /**
     * Target response data.
     *
     * @param targetId opaque target
     * @param domainId authority domain
     * @param namespaceId namespace id
     * @param selectorId selector id
     * @param protocol upstream protocol
     * @param host upstream host
     * @param port upstream port
     * @param generation generation high-water mark
     * @param state target state
     */
    public record TargetData(String targetId, String domainId, String namespaceId, String selectorId,
                             String protocol, String host, Integer port, String generation, String state) {
    }

    /**
     * Mutation result data.
     *
     * @param generation generation
     * @param clientSequence committed sequence
     * @param operation operation name
     * @param result operation result
     */
    public record MutationData(String generation, String clientSequence, String operation, String result) {
    }
}
