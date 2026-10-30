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

package org.apache.shenyu.admin.controller;

import org.apache.shenyu.admin.service.fencing.RegistrationFencingException;
import org.apache.shenyu.admin.config.properties.RegistrationFencingProperties;
import org.apache.shenyu.admin.service.fencing.RegistrationFencingService;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;
import org.apache.shenyu.register.common.protocol.RegistrationProtocol;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

/**
 * Opt-in, synchronous business registration fencing endpoints.
 */
@RestController
@RequestMapping
public class RegistrationFencingController {

    private final RegistrationFencingService service;

    private final RegistrationFencingProperties properties;

    /**
     * Create a controller.
     *
     * @param service fencing service
     * @param properties fencing properties
     */
    public RegistrationFencingController(final RegistrationFencingService service,
                                         final RegistrationFencingProperties properties) {
        this.service = service;
        this.properties = properties;
    }

    /**
     * Pre-provision an exact target.
     *
     * @param version protocol version
     * @param request target request
     * @return target response
     */
    @PostMapping(RegistrationFencingProtocol.TARGETS_PATH)
    public ResponseEntity<RegistrationFencingProtocol.Response<RegistrationFencingProtocol.TargetData>> createTarget(
            @RequestHeader(value = RegistrationProtocol.VERSION_HEADER, required = false) final String version,
            @RequestBody final RegistrationFencingProtocol.TargetRequest request) {
        return execute(version, () -> service.createTarget(request, SessionUtil.isAdmin()));
    }

    /**
     * Explicitly replace a target owner.
     *
     * @param version protocol version
     * @param targetId target id
     * @param request replacement request
     * @return session response
     */
    @PostMapping(RegistrationFencingProtocol.REPLACE_PATH)
    public ResponseEntity<RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData>> replace(
            @RequestHeader(value = RegistrationProtocol.VERSION_HEADER, required = false) final String version,
            @PathVariable final String targetId,
            @RequestBody final RegistrationFencingProtocol.ReplaceRequest request) {
        return execute(version, () -> service.replace(targetId, request, SessionUtil.isAdmin()));
    }

    /**
     * Disable a target and terminate its session.
     *
     * @param version protocol version
     * @param targetId target id
     * @return target response
     */
    @PostMapping(RegistrationFencingProtocol.DISABLE_PATH)
    public ResponseEntity<RegistrationFencingProtocol.Response<RegistrationFencingProtocol.TargetData>> disable(
            @RequestHeader(value = RegistrationProtocol.VERSION_HEADER, required = false) final String version,
            @PathVariable final String targetId) {
        return execute(version, () -> service.disable(targetId, SessionUtil.isAdmin()));
    }

    /**
     * Acquire a pre-provisioned target.
     *
     * @param version protocol version
     * @param request acquire request
     * @return session response
     */
    @PostMapping(RegistrationFencingProtocol.ACQUIRE_PATH)
    public ResponseEntity<RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData>> acquire(
            @RequestHeader(value = RegistrationProtocol.VERSION_HEADER, required = false) final String version,
            @RequestBody final RegistrationFencingProtocol.AcquireRequest request) {
        return execute(version, () -> service.acquire(request, SessionUtil.visitorId()));
    }

    /**
     * Apply one synchronous fenced mutation.
     *
     * @param version protocol version
     * @param sessionId session id in path
     * @param request mutation request
     * @return mutation response
     */
    @PostMapping(RegistrationFencingProtocol.MUTATE_PATH)
    public ResponseEntity<RegistrationFencingProtocol.Response<RegistrationFencingProtocol.MutationData>> mutate(
            @RequestHeader(value = RegistrationProtocol.VERSION_HEADER, required = false) final String version,
            @PathVariable final String sessionId,
            @RequestBody final RegistrationFencingProtocol.MutationRequest request) {
        RegistrationFencingProtocol.MutationRequest pathRequest = request;
        if (Objects.nonNull(request) && !sessionId.equals(request.sessionId())) {
            pathRequest = new RegistrationFencingProtocol.MutationRequest(request.domainId(), sessionId,
                    request.selectorId(), request.generation(), request.clientSequence(), request.operation(), request.payload());
        }
        final RegistrationFencingProtocol.MutationRequest finalRequest = pathRequest;
        return execute(version, () -> service.mutate(finalRequest, SessionUtil.visitorId()));
    }

    private <T> ResponseEntity<RegistrationFencingProtocol.Response<T>> execute(final String version,
                                                                                  final Operation<T> operation) {
        if (!properties.isEnabled()) {
            return response(RegistrationFencingProtocol.Response.error(
                    RegistrationFencingProtocol.FENCING_NOT_ENABLED, "fencing is not enabled"));
        }
        if (!RegistrationProtocol.V1.equals(version)) {
            return response(RegistrationFencingProtocol.Response.error(
                    RegistrationFencingProtocol.INVALID_REQUEST, "protocol version 1 is required"));
        }
        try {
            return response(operation.run());
        } catch (RegistrationFencingException exception) {
            return response(RegistrationFencingProtocol.Response.error(exception.getCode(), exception.getMessage()));
        }
    }

    private <T> ResponseEntity<RegistrationFencingProtocol.Response<T>> response(
            final RegistrationFencingProtocol.Response<T> response) {
        HttpStatus status = switch (response.code()) {
            case RegistrationFencingProtocol.INVALID_REQUEST -> HttpStatus.BAD_REQUEST;
            case RegistrationFencingProtocol.FORBIDDEN -> HttpStatus.FORBIDDEN;
            case RegistrationFencingProtocol.TARGET_NOT_FOUND -> HttpStatus.NOT_FOUND;
            case RegistrationFencingProtocol.FENCING_NOT_ENABLED -> HttpStatus.SERVICE_UNAVAILABLE;
            case "OK" -> HttpStatus.OK;
            default -> HttpStatus.CONFLICT;
        };
        return ResponseEntity.status(status).body(response);
    }

    @FunctionalInterface
    private interface Operation<T> {

        /**
         * Execute an operation.
         *
         * @return response
         */
        RegistrationFencingProtocol.Response<T> run();
    }
}
