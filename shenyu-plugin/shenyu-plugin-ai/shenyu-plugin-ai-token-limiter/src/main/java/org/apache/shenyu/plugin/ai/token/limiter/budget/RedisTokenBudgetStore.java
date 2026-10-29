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

package org.apache.shenyu.plugin.ai.token.limiter.budget;

import org.springframework.core.io.ClassPathResource;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.data.redis.core.script.RedisScript;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.List;
import java.util.Objects;

/**
 * Atomic budget admission and completion for a caller-selected scope and immutable window.
 * Unknown upstream usage retains its reservation until reliable settlement or window expiry.
 * Fixed windows use Redis time; gateway request lifecycle integration is handled separately.
 */
public final class RedisTokenBudgetStore {

    private static final long MAX_EXACT_INTEGER = 9_007_199_254_740_991L;

    private static final RedisScript<Long> RESERVE_SCRIPT = createScript("ai_token_reserve.lua");

    private static final RedisScript<Long> SETTLE_SCRIPT = createScript("ai_token_settle.lua");

    private static final RedisScript<Long> COMPLETE_WITHOUT_USAGE_SCRIPT = createScript("ai_token_complete_without_usage.lua");

    private static final RedisScript<Long> TIME_SCRIPT = createScript("ai_token_time.lua");

    private final ReactiveRedisTemplate<String, String> redisTemplate;

    public RedisTokenBudgetStore(final ReactiveRedisTemplate<String, String> redisTemplate) {
        this.redisTemplate = Objects.requireNonNull(redisTemplate, "redisTemplate");
    }

    /**
     * Select an epoch-aligned fixed window with the Redis server clock.
     * Selection does not create budget state. Admission must recheck its boundaries atomically.
     *
     * @param scopeKey rule and resolved identity scope
     * @param windowSize positive window size in whole milliseconds
     * @return immutable scope/window identity for reservation and completion
     */
    public Mono<TokenBudgetWindow> currentWindow(final String scopeKey, final Duration windowSize) {
        return Mono.defer(() -> {
            final long windowMillis = TokenBudgetWindow.validateWindowSize(scopeKey, windowSize);
            return redisTemplate.execute(TIME_SCRIPT, List.of(TokenBudgetWindow.scopePrefix(scopeKey)), List.of()).single()
                    .map(now -> TokenBudgetWindow.at(scopeKey, now, windowMillis));
        });
    }

    /**
     * Reserve within a fixed window, checking Redis time before writing any state.
     * A WINDOW_NOT_CURRENT result requires selecting a new window before retrying admission.
     *
     * @param window immutable window selected for this request
     * @param requestId unique request identity retained across retries
     * @param limit window token budget
     * @param amount per-request reservation
     * @return admission decision; expired windows are never recreated
     */
    public Mono<ReservationResult> reserve(final TokenBudgetWindow window, final String requestId, final long limit, final long amount) {
        return Mono.defer(() -> {
            if (Objects.isNull(window) || Objects.isNull(requestId) || requestId.isBlank() || !isValidInteger(limit) || !isValidInteger(amount)) {
                return Mono.error(new IllegalArgumentException("A window, request identity and positive exact token amounts must be provided"));
            }
            final List<String> args = List.of(Long.toString(limit), Long.toString(amount), Long.toString(window.getEndMillis() - window.getStartMillis()),
                    requestId, Long.toString(window.getStartMillis()), Long.toString(window.getEndMillis()));
            return redisTemplate.execute(RESERVE_SCRIPT, List.of(window.getKey()), args).single().map(ReservationResult::fromCode);
        });
    }

    /**
     * Low-level admission primitive for package-local script tests with caller-owned window lifetimes.
     * Gateway callers must use the fixed-window overload, which validates Redis time before admission.
     *
     * @param windowKey budget hash key scoped to one identity and one window
     * @param requestId unique request identity retained across retries
     * @param limit token budget for the window
     * @param amount requested reservation
     * @param remainingWindowTtl remaining window lifetime, used only when creating its hash
     * @return a decision, or an error when input or Redis state is invalid
     */
    Mono<ReservationResult> reserve(final String windowKey, final String requestId, final long limit,
                                    final long amount, final Duration remainingWindowTtl) {
        return Mono.defer(() -> {
            if (Objects.isNull(windowKey) || windowKey.isBlank() || Objects.isNull(requestId) || requestId.isBlank()) {
                return Mono.error(new IllegalArgumentException("Window key and request ID must be nonblank"));
            }
            if (Objects.isNull(remainingWindowTtl)) {
                return Mono.error(new IllegalArgumentException("Window TTL must be provided"));
            }
            final long ttlMillis;
            try {
                ttlMillis = remainingWindowTtl.toMillis();
            } catch (ArithmeticException exception) {
                return Mono.error(new IllegalArgumentException("Window TTL is out of range", exception));
            }
            if (!isValidInteger(limit) || !isValidInteger(amount) || !isValidInteger(ttlMillis)) {
                return Mono.error(new IllegalArgumentException("Budget, reservation and TTL must be positive exact Lua integers"));
            }
            final List<String> args = List.of(Long.toString(limit), Long.toString(amount), Long.toString(ttlMillis), requestId);
            return redisTemplate.execute(RESERVE_SCRIPT, List.of(windowKey), args).single().map(ReservationResult::fromCode);
        });
    }

    private boolean isValidInteger(final long value) {
        return value > 0 && value <= MAX_EXACT_INTEGER;
    }

    /**
     * Convert a reservation into known final usage exactly once, retaining its original window.
     * Repeated calls must carry the same actual usage. Missing windows are never recreated.
     *
     * @param windowKey original scope/window hash key returned to the caller at admission
     * @param requestId original reservation identity
     * @param actualTokens known final usage, including zero
     * @return a settlement outcome, or an error for invalid input, conflicting usage or Redis state
     */
    public Mono<SettlementResult> settle(final String windowKey, final String requestId, final long actualTokens) {
        return Mono.defer(() -> {
            if (Objects.isNull(windowKey) || windowKey.isBlank() || Objects.isNull(requestId) || requestId.isBlank()) {
                return Mono.error(new IllegalArgumentException("Window key and request ID must be nonblank"));
            }
            if (actualTokens < 0 || actualTokens > MAX_EXACT_INTEGER) {
                return Mono.error(new IllegalArgumentException("Actual usage must be a nonnegative exact Lua integer"));
            }
            return redisTemplate.execute(SETTLE_SCRIPT, List.of(windowKey), List.of(Long.toString(actualTokens), requestId))
                    .single().map(SettlementResult::fromCode);
        });
    }

    /**
     * Complete a request without reported usage, releasing only confirmed unforwarded requests.
     * An error, cancellation or missing usage after possible forwarding must use USAGE_UNKNOWN.
     * Held reservations may later be settled with reliable usage; completion never extends the window.
     *
     * @param windowKey original scope/window hash key
     * @param requestId original reservation identity
     * @param outcome whether upstream execution was ruled out or usage remains unknown
     * @return an idempotent completion result, or an error for invalid input or state
     */
    public Mono<CompletionResult> completeWithoutUsage(final String windowKey, final String requestId, final UsageOutcome outcome) {
        return Mono.defer(() -> {
            if (Objects.isNull(windowKey) || windowKey.isBlank() || Objects.isNull(requestId) || requestId.isBlank() || Objects.isNull(outcome)) {
                return Mono.error(new IllegalArgumentException("Window key, request ID and usage outcome must be provided"));
            }
            return redisTemplate.execute(COMPLETE_WITHOUT_USAGE_SCRIPT, List.of(windowKey), List.of(requestId, outcome.name()))
                    .single().map(CompletionResult::fromCode);
        });
    }

    private static RedisScript<Long> createScript(final String filename) {
        final DefaultRedisScript<Long> script = new DefaultRedisScript<>();
        script.setLocation(new ClassPathResource("META-INF/scripts/" + filename));
        script.setResultType(Long.class);
        return script;
    }

    /**
     * Outcome of one budget reservation attempt.
     */
    public enum ReservationResult {

        /** Insufficient available budget. */
        REJECTED,

        /** Budget newly reserved for this request. */
        RESERVED,

        /** This request already owns the same reservation. */
        ALREADY_RESERVED,

        /** This request has already completed settlement and must not reserve again. */
        ALREADY_SETTLED,

        /** This request was confirmed unforwarded and its reservation was released. */
        ALREADY_RELEASED,

        /** Upstream usage remains unknown; this request must not be forwarded again. */
        USAGE_UNKNOWN,

        /** Redis time is outside the selected window; select another window before retrying. */
        WINDOW_NOT_CURRENT;

        private static ReservationResult fromCode(final long code) {
            if (code == 0L) {
                return REJECTED;
            }
            if (code == 1L) {
                return RESERVED;
            }
            if (code == 2L) {
                return ALREADY_RESERVED;
            }
            if (code == 3L) {
                return ALREADY_SETTLED;
            }
            if (code == 4L) {
                return ALREADY_RELEASED;
            }
            if (code == 5L) {
                return USAGE_UNKNOWN;
            }
            if (code == 6L) {
                return WINDOW_NOT_CURRENT;
            }
            throw new IllegalStateException("Unexpected token reservation result: " + code);
        }
    }

    /**
     * Outcome of one known-usage settlement attempt.
     */
    public enum SettlementResult {

        /** The live window has no reservation for this request. */
        RESERVATION_NOT_FOUND,

        /** The reservation was converted into actual usage. */
        SETTLED,

        /** This request was previously settled with the same actual usage. */
        ALREADY_SETTLED,

        /** The original window no longer exists; no state was created. */
        WINDOW_NOT_FOUND;

        private static SettlementResult fromCode(final long code) {
            if (code == 0L) {
                return RESERVATION_NOT_FOUND;
            }
            if (code == 1L) {
                return SETTLED;
            }
            if (code == 2L) {
                return ALREADY_SETTLED;
            }
            if (code == 3L) {
                return WINDOW_NOT_FOUND;
            }
            throw new IllegalStateException("Unexpected token settlement result: " + code);
        }
    }

    /**
     * Evidence available when a request completes without reported usage.
     */
    public enum UsageOutcome {

        /** Caller has confirmed that the request never reached upstream execution. */
        NOT_FORWARDED,

        /** Upstream execution is possible but final usage is unavailable. */
        USAGE_UNKNOWN
    }

    /**
     * Outcome of completing a request without reported usage.
     */
    public enum CompletionResult {

        /** The live window has no reservation for this request. */
        RESERVATION_NOT_FOUND,

        /** A confirmed unforwarded request was released, possibly by an earlier call. */
        RELEASED,

        /** The original reservation remains held for unknown upstream usage. */
        USAGE_HELD,

        /** Reliable final usage has already been recorded; nothing was changed. */
        ALREADY_SETTLED,

        /** The original window no longer exists; no state was created. */
        WINDOW_NOT_FOUND;

        private static CompletionResult fromCode(final long code) {
            if (code == 0L) {
                return RESERVATION_NOT_FOUND;
            }
            if (code == 1L) {
                return RELEASED;
            }
            if (code == 2L) {
                return USAGE_HELD;
            }
            if (code == 3L) {
                return ALREADY_SETTLED;
            }
            if (code == 4L) {
                return WINDOW_NOT_FOUND;
            }
            throw new IllegalStateException("Unexpected token completion result: " + code);
        }
    }
}
