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

package org.apache.shenyu.admin.utils;

import jakarta.servlet.http.HttpServletRequest;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.servlet.HandlerMapping;

import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.LongSupplier;

/**
 * Body-free Admin security observations. This class never makes an authorization decision.
 */
public final class AdminSecurityAudit {

    public static final String REQUEST_ID_HEADER = "X-Shenyu-Admin-Request-Id";

    static final int FAILURE_LIMIT = 10;

    private static final String REQUEST_ID = AdminSecurityAudit.class.getName() + ".requestId";

    private static final String DENIAL_RECORDED = AdminSecurityAudit.class.getName() + ".denialRecorded";

    private static final String DENIAL_EXCEPTION = AdminSecurityAudit.class.getName() + ".denialException";

    private static final Logger LOG = LoggerFactory.getLogger("org.apache.shenyu.admin.security.audit");

    private static final AdminSecurityAudit INSTANCE = new AdminSecurityAudit(System::nanoTime, message -> LOG.info("{}", message));

    private final Map<Reason, Budget> budgets = new EnumMap<>(Reason.class);

    private final LongSupplier clock;

    private final Consumer<String> sink;

    AdminSecurityAudit(final LongSupplier clock, final Consumer<String> sink) {
        this.clock = clock;
        this.sink = sink;
        for (Reason reason : Reason.values()) {
            budgets.put(reason, new Budget(clock.getAsLong()));
        }
    }

    /**
     * Generate a server-owned ID, never accepting a supplied header as audit identity.
     *
     * @param request servlet request
     * @return request correlation ID
     */
    public static String requestId(final HttpServletRequest request) {
        if (Objects.isNull(request)) {
            return UUID.randomUUID().toString();
        }
        Object existing = request.getAttribute(REQUEST_ID);
        if (existing instanceof String) {
            return (String) existing;
        }
        String generated = UUID.randomUUID().toString();
        request.setAttribute(REQUEST_ID, generated);
        return generated;
    }

    /**
     * Capture servlet correlation for existing operation events before asynchronous dispatch.
     *
     * @return request ID, or null for a non-servlet event
     */
    public static String currentRequestId() {
        HttpServletRequest request = currentRequest();
        return Objects.isNull(request) ? null : requestId(request);
    }

    /**
     * Check whether a typed policy denial already owns this request's bounded observation.
     *
     * @param exception exception being handled
     * @return true only for the observed exception or a wrapper with that direct cause
     */
    public static boolean isDenialRecorded(final Throwable exception) {
        HttpServletRequest request = currentRequest();
        if (Objects.isNull(request)) {
            return false;
        }
        Object recorded = request.getAttribute(DENIAL_EXCEPTION);
        return Objects.nonNull(recorded) && (recorded == exception || recorded == exception.getCause());
    }

    /**
     * Attach an observation to the exact policy exception while preserving its type and message.
     *
     * @param exception original policy exception
     * @param reason bounded reason
     * @param action action identifier
     * @param namespace namespace ID
     * @param resource resource ID
     * @param <T> original exception type
     * @return the unchanged exception
     */
    public static <T extends RuntimeException> T denied(final T exception, final Reason reason, final String action,
                                                       final String namespace, final String resource) {
        denied(reason, action, namespace, resource);
        HttpServletRequest request = currentRequest();
        if (Objects.nonNull(request)) {
            request.setAttribute(DENIAL_EXCEPTION, exception);
        }
        return exception;
    }

    /**
     * Record a denied action once per servlet request, using only explicit identity fields.
     *
     * @param reason bounded reason
     * @param action action identifier, or null for the MVC route template
     * @param namespace namespace ID when known
     * @param resource resource ID when known
     */
    public static void denied(final Reason reason, final String action, final String namespace, final String resource) {
        HttpServletRequest request = currentRequest();
        if (Objects.nonNull(request)) {
            if (Boolean.TRUE.equals(request.getAttribute(DENIAL_RECORDED))) {
                return;
            }
            request.setAttribute(DENIAL_RECORDED, Boolean.TRUE);
        }
        INSTANCE.record(reason, request, actor(), action, namespace, resource);
    }

    /**
     * Record failure without trusting claims from the rejected credential.
     *
     * @param request request, or null outside servlet processing
     * @param reason bounded failure category
     */
    public static void authenticationFailed(final HttpServletRequest request, final Reason reason) {
        INSTANCE.record(reason, Objects.nonNull(request) ? request : currentRequest(), "unknown", "authenticate", null, null);
    }

    /**
     * Record successful token-version revocation only after its transaction commits.
     * Identity is captured now, before transaction callbacks can lose servlet/Shiro context.
     *
     * @param userId target dashboard account ID
     */
    public static void credentialsRevoked(final String userId) {
        INSTANCE.revoked(currentRequest(), actor(), userId);
    }

    void revoked(final HttpServletRequest request, final String actorId, final String userId) {
        Map<String, Object> event = event(Reason.CREDENTIAL_REVOKED, request, actorId, "revoke_tokens", null, userId);
        if (TransactionSynchronizationManager.isActualTransactionActive() && TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                @Override
                public void afterCommit() {
                    emit(event);
                }
            });
        } else {
            emit(event);
        }
    }

    void record(final Reason reason, final HttpServletRequest request, final String actorId, final String action,
                final String namespace, final String resource) {
        long suppressed = budgets.get(reason).acquire(clock.getAsLong());
        if (suppressed < 0) {
            return;
        }
        Map<String, Object> event = event(reason, request, actorId, action, namespace, resource);
        event.put("suppressed_since_previous", suppressed);
        emit(event);
    }

    private Map<String, Object> event(final Reason reason, final HttpServletRequest request, final String actorId,
                                      final String action, final String namespace, final String resource) {
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("schema", 1);
        result.put("timestamp_ms", System.currentTimeMillis());
        result.put("request_id", requestId(request));
        result.put("actor_id", bounded(actorId));
        result.put("action", bounded(Objects.nonNull(action) ? action : route(request)));
        result.put("namespace_id", bounded(namespace));
        result.put("resource_id", bounded(resource));
        result.put("outcome", reason == Reason.CREDENTIAL_REVOKED ? "success" : "denied");
        result.put("reason", reason.name());
        return result;
    }

    private void emit(final Map<String, Object> event) {
        try {
            sink.accept(GsonUtils.getInstance().toJson(event));
        } catch (RuntimeException ignored) {
            // Observations cannot turn a denial into success or fail an already committed mutation.
            // Do not recursively log a sink failure, which would bypass the volume bound.
        }
    }

    private static String bounded(final String value) {
        return Objects.isNull(value) ? "unknown" : value.substring(0, Math.min(value.length(), 128));
    }

    private static HttpServletRequest currentRequest() {
        if (RequestContextHolder.getRequestAttributes() instanceof ServletRequestAttributes) {
            return ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();
        }
        return null;
    }

    private static String actor() {
        Subject subject = ThreadContext.getSubject();
        if (Objects.nonNull(subject) && subject.isAuthenticated() && subject.getPrincipal() instanceof UserInfo) {
            return ((UserInfo) subject.getPrincipal()).getUserId();
        }
        return "unknown";
    }

    private static String route(final HttpServletRequest request) {
        if (Objects.isNull(request)) {
            return "unknown";
        }
        Object pattern = request.getAttribute(HandlerMapping.BEST_MATCHING_PATTERN_ATTRIBUTE);
        return pattern instanceof String ? (String) pattern : "unknown";
    }

    /**
     * Fixed categories also bound limiter state; no token, IP, user or namespace keyed map.
     */
    public enum Reason {
        MISSING_TOKEN,
        REJECTED_TOKEN,
        LOGIN_FAILED,
        ACTION_DENIED,
        NAMESPACE_DENIED,
        RESOURCE_DENIED,
        SERVICE_IDENTITY_DENIED,
        CREDENTIAL_REVOKED
    }

    private static final class Budget {

        private long start;

        private int used;

        private long suppressed;

        private Budget(final long now) {
            this.start = now;
        }

        private synchronized long acquire(final long now) {
            if (now - start >= TimeUnit.SECONDS.toNanos(1)) {
                start = now;
                used = 0;
            }
            if (used >= FAILURE_LIMIT) {
                if (suppressed < Long.MAX_VALUE) {
                    suppressed++;
                }
                return -1;
            }
            used++;
            long previous = suppressed;
            suppressed = 0;
            return previous;
        }
    }
}
