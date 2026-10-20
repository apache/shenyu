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

package org.apache.shenyu.admin.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.model.dto.AppAuthApplyRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthReplacePathsRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthResultQueryDTO;
import org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.AppAuthLifecycleService;
import org.apache.shenyu.admin.service.AppAuthAccessPolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.CannotAcquireLockException;
import org.springframework.dao.DeadlockLoserDataAccessException;
import org.springframework.stereotype.Service;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.stream.Collectors;

/**
 * AppAuthLifecycleService facade impl.
 * The facade holds no outer write transaction: authenticate/authorize -> canonical digest -> call the transaction bean via proxy;
 * only database deadlock/lock-wait gets a bounded retry outside the transaction (after the old one rolled back); parameter conflicts and REVOKED never retry.
 */
@Service
public class AppAuthLifecycleServiceImpl implements AppAuthLifecycleService {

    private static final Logger LOG = LoggerFactory.getLogger(AppAuthLifecycleServiceImpl.class);

    private static final int MAX_RETRY_ATTEMPTS = 3;

    /** Canonical serializer (sorted keys via TreeMap; nulls included; no extra config). */
    private static final ObjectMapper CANONICAL_MAPPER = new ObjectMapper();

    private final AppAuthAccessPolicy accessPolicy;

    private final AppAuthTransactionService transactionService;

    public AppAuthLifecycleServiceImpl(final AppAuthAccessPolicy accessPolicy,
                                          final AppAuthTransactionService transactionService) {
        this.accessPolicy = accessPolicy;
        this.transactionService = transactionService;
    }

    @Override
    public ShenyuAdminResult apply(final AppAuthApplyRequestDTO request) {
        final String callerKey = accessPolicy.requireServiceCaller("system:authen:lifecycleApply");
        accessPolicy.requireNamespace(callerKey, request.getNamespaceId());
        final String requestHash = canonicalRequestHash(request);
        return withDeadlockRetry("apply", request.getRequestId(),
                () -> ShenyuAdminResult.success(transactionService.apply(callerKey, request, requestHash)));
    }

    @Override
    public ShenyuAdminResult result(final AppAuthResultQueryDTO query) {
        final String callerKey = accessPolicy.requireServiceCaller("system:authen:lifecycleResult");
        accessPolicy.requireNamespace(callerKey, query.getNamespaceId());
        return ShenyuAdminResult.success(transactionService.result(callerKey, query));
    }

    @Override
    public ShenyuAdminResult revoke(final AppAuthRevokeRequestDTO request) {
        final String callerKey = accessPolicy.requireServiceCaller("system:authen:lifecycleRevoke");
        accessPolicy.requireNamespace(callerKey, request.getNamespaceId());
        return withDeadlockRetry("revoke", request.getRequestId(),
                () -> ShenyuAdminResult.success(transactionService.revoke(callerKey, request)));
    }

    @Override
    public ShenyuAdminResult replacePaths(final AppAuthReplacePathsRequestDTO request) {
        final String callerKey = accessPolicy.requireServiceCaller("system:authen:lifecycleReplacePaths");
        accessPolicy.requireNamespace(callerKey, request.getNamespaceId());
        accessPolicy.requireOwnedAuth(callerKey, request.getNamespaceId(), request.getAuthId());
        return withDeadlockRetry("replacePaths", request.getAuthId(),
                () -> ShenyuAdminResult.success(transactionService.replacePaths(callerKey, request)));
    }

    /**
     * Canonical request digest: sorted keys, de-duplicated and sorted pathList, trimmed appName, explicit nulls
     * for optional fields, fixed open=true/enabled=false; requestId/caller/credentials excluded.
     * Test vector: src/test/resources/app-auth/canonical-request.json (shared with the iPaaS side).
     *
     * @param request apply request
     * @return SHA-256 hex
     */
    static String canonicalRequestHash(final AppAuthApplyRequestDTO request) {
        final Map<String, Object> canonical = new TreeMap<>();
        canonical.put("appName", ObjectsUtil.trimToNull(request.getAppName()));
        canonical.put("appParam", request.getAppParam());
        canonical.put("enabled", Boolean.FALSE);
        canonical.put("extInfo", request.getExtInfo());
        canonical.put("namespaceId", request.getNamespaceId());
        canonical.put("open", Boolean.TRUE);
        canonical.put("pathList", Objects.isNull(request.getPathList())
                ? List.of()
                : request.getPathList().stream().distinct().sorted().collect(Collectors.toList()));
        canonical.put("phone", request.getPhone());
        canonical.put("userId", request.getUserId());
        try {
            final byte[] json = CANONICAL_MAPPER.writeValueAsBytes(canonical);
            return sha256Hex(json);
        } catch (JsonProcessingException e) {
            throw new ShenyuAdminException("Failed to normalize the request");
        }
    }

    private static String sha256Hex(final byte[] data) {
        try {
            final MessageDigest digest = MessageDigest.getInstance("SHA-256");
            final byte[] hash = digest.digest(data);
            final StringBuilder sb = new StringBuilder(hash.length * 2);
            for (final byte b : hash) {
                sb.append(Character.forDigit((b >> 4) & 0xF, 16));
                sb.append(Character.forDigit(b & 0xF, 16));
            }
            return sb.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("SHA-256 unavailable", e);
        }
    }

    private ShenyuAdminResult withDeadlockRetry(final String action, final String requestId,
                                                final java.util.function.Supplier<ShenyuAdminResult> call) {
        ShenyuAdminException last = null;
        for (int attempt = 1; attempt <= MAX_RETRY_ATTEMPTS; attempt++) {
            try {
                return call.get();
            } catch (DeadlockLoserDataAccessException | CannotAcquireLockException e) {
                // reached only after the proxy rolled the transaction back; retry the whole transaction a bounded number of times
                last = new ShenyuAdminException("Operation failed due to database lock contention, please retry");
                LOG.warn("FLM_APP_AUTH_RETRY: action={}, requestId={}, attempt={}/{}",
                        action, requestId, attempt, MAX_RETRY_ATTEMPTS);
            }
        }
        throw last;
    }

    /** Minimal internal object utils (avoids a new dependency for two null checks). */
    private static final class ObjectsUtil {

        private ObjectsUtil() {
        }

        private static String trimToNull(final String value) {
            if (Objects.isNull(value)) {
                return null;
            }
            final String trimmed = value.trim();
            return trimmed.isEmpty() ? null : trimmed;
        }
    }
}
