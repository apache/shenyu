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

package org.apache.shenyu.plugin.divide.sticky;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.rule.sticky.StickySessionConfig;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.HexFormat;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Explicit sticky session affinity for HTTP proxying, composed with the
 * configured load-balancing algorithm (#8037): the algorithm selects the
 * target for a new session, and this layer reuses a previously selected
 * target while it stays inside the admitted candidate set.
 *
 * <p>The binding travels in a gateway-issued, HMAC-signed cookie scoped to
 * the rule; it never bypasses canary partitions, health or circuit-breaker
 * admission, because the bound target is only honored when it is still a
 * member of the already-filtered candidate list.</p>
 */
public class StickySessionAffinity {

    private static final Logger LOG = LoggerFactory.getLogger(StickySessionAffinity.class);

    private static final String POLICY_STRICT = "STRICT";

    private static final String HMAC_ALGORITHM = "HmacSHA256";

    private static final String SEPARATOR = "|";

    /**
     * Bytes of the rule-id digest used in the default cookie name: 12 hex
     * characters, enough to keep distinct rules apart while staying readable.
     */
    private static final int RULE_DIGEST_BYTES = 6;

    private static final Base64.Encoder ENCODER = Base64.getUrlEncoder().withoutPadding();

    private static final Base64.Decoder DECODER = Base64.getUrlDecoder();

    /**
     * Whether the rule carries an active sticky configuration. Blank secrets,
     * a blank cookie name or a non-positive binding lifetime disable affinity
     * instead of producing unusable cookies.
     *
     * @param handle the sticky configuration, may be null
     * @return true when affinity applies to the request
     */
    public boolean isActive(final StickySessionConfig handle) {
        if (Objects.isNull(handle) || !handle.isEnabled() || handle.getMaxAgeSeconds() <= 0
                || StringUtils.isBlank(handle.getCookieName())
                || Objects.isNull(handle.getSecrets()) || handle.getSecrets().isEmpty()) {
            return false;
        }
        return handle.getSecrets().stream().allMatch(StringUtils::isNotBlank);
    }

    /**
     * Resolves the browser cookie that stores the binding of a rule. The
     * default name is a base name and the effective name carries a rule-scoped
     * suffix, so rules served under the same host never overwrite each other's
     * binding (#8136). Any other name is used verbatim: rules configuring the
     * same name share one cookie, and the signed rule id still makes a foreign
     * binding a miss.
     *
     * @param ruleId the identifier of the matched rule, the binding scope
     * @param handle the sticky configuration
     * @return the effective cookie name
     */
    public String cookieName(final String ruleId, final StickySessionConfig handle) {
        final String configured = handle.getCookieName();
        if (StringUtils.isNotBlank(configured) && !StickySessionConfig.DEFAULT_COOKIE_NAME.equals(configured)) {
            return configured;
        }
        return StickySessionConfig.DEFAULT_COOKIE_NAME + "-" + ruleDigest(ruleId);
    }

    private String ruleDigest(final String ruleId) {
        try {
            // a digest keeps the name within the cookie-name charset whatever the rule id holds
            final byte[] digest = MessageDigest.getInstance("SHA-256").digest(ruleId.getBytes(StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(digest, 0, RULE_DIGEST_BYTES);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("sticky session affinity digest failure", e);
        }
    }

    /**
     * Resolves the bound target for the request. The returned upstream is a
     * member of the given candidate list, which must already be narrowed by
     * canary partitioning and circuit-breaker admission.
     *
     * @param exchange the current server exchange
     * @param ruleId the identifier of the matched rule, the binding scope
     * @param handle the sticky configuration
     * @param candidates the admitted upstream candidates
     * @return the resolution: an eligible bound target and whether a binding was presented
     */
    public Resolution resolve(final ServerWebExchange exchange, final String ruleId,
                              final StickySessionConfig handle, final List<Upstream> candidates) {
        final String cookieValue = readCookie(exchange.getRequest(), cookieName(ruleId, handle));
        if (StringUtils.isBlank(cookieValue)) {
            return new Resolution(null, false);
        }
        final Optional<String> hashKey = decode(cookieValue, ruleId, handle);
        if (!hashKey.isPresent()) {
            // a broken, foreign-rule or expired cookie starts a new session
            return new Resolution(null, false);
        }
        return new Resolution(candidates.stream()
                .filter(upstream -> upstream.identity().hashKey().equals(hashKey.get()))
                .findFirst()
                .orElse(null), true);
    }

    /**
     * Whether the strict failure policy applies to the rule.
     *
     * @param handle the sticky configuration
     * @return true when an unavailable bound target must reject the request
     */
    public boolean isStrict(final StickySessionConfig handle) {
        return POLICY_STRICT.equals(handle.getOnUnavailable());
    }

    /**
     * Writes the binding cookie for the final target. Called after the target
     * is selected and before the response headers are committed, so a retry to
     * a different target downstream never rewrites the binding.
     *
     * @param exchange the current server exchange
     * @param ruleId the identifier of the matched rule, the binding scope
     * @param upstream the target the binding points at
     * @param handle the sticky configuration
     */
    public void writeCookie(final ServerWebExchange exchange, final String ruleId,
                            final Upstream upstream, final StickySessionConfig handle) {
        final long expiry = System.currentTimeMillis() / 1000L + handle.getMaxAgeSeconds();
        final String payload = ruleId + SEPARATOR + upstream.identity().hashKey() + SEPARATOR + expiry;
        final ServerHttpResponse response = exchange.getResponse();
        final ResponseCookie cookie = ResponseCookie.from(cookieName(ruleId, handle), encode(payload, handle.getSecrets().get(0)))
                .httpOnly(true)
                .secure("https".equalsIgnoreCase(exchange.getRequest().getURI().getScheme()))
                .path("/")
                .sameSite("Lax")
                .maxAge(handle.getMaxAgeSeconds())
                .build();
        response.getHeaders().add(HttpHeaders.SET_COOKIE, cookie.toString());
        LOG.debug("sticky session affinity bound rule {} to {}", ruleId, upstream.identity().hashKey());
    }

    private String readCookie(final ServerHttpRequest request, final String cookieName) {
        return Objects.nonNull(request.getCookies().getFirst(cookieName))
                ? request.getCookies().getFirst(cookieName).getValue() : null;
    }

    private Optional<String> decode(final String cookieValue, final String ruleId, final StickySessionConfig handle) {
        final int separator = cookieValue.indexOf('.');
        if (separator <= 0 || separator == cookieValue.length() - 1) {
            return Optional.empty();
        }
        final String payloadPart = cookieValue.substring(0, separator);
        final String signaturePart = cookieValue.substring(separator + 1);
        final String payload = verify(payloadPart, signaturePart, handle);
        if (Objects.isNull(payload)) {
            LOG.debug("sticky session cookie rejected: signature or format invalid");
            return Optional.empty();
        }
        final String[] parts = payload.split("\\" + SEPARATOR, -1);
        if (parts.length != 3 || !ruleId.equals(parts[0])) {
            // a cookie of another rule must not bind this one
            return Optional.empty();
        }
        final long expiry;
        try {
            expiry = Long.parseLong(parts[2]);
        } catch (NumberFormatException e) {
            return Optional.empty();
        }
        if (expiry < System.currentTimeMillis() / 1000L) {
            return Optional.empty();
        }
        return Optional.ofNullable(parts[1]).filter(hashKey -> !hashKey.isEmpty());
    }

    private String verify(final String payloadPart, final String signaturePart, final StickySessionConfig handle) {
        final byte[] signature;
        final byte[] payload;
        try {
            signature = DECODER.decode(signaturePart);
            payload = DECODER.decode(payloadPart);
        } catch (IllegalArgumentException e) {
            return null;
        }
        for (final String secret : handle.getSecrets()) {
            if (MessageDigest.isEqual(signature, hmac(new String(payload, StandardCharsets.UTF_8), secret))) {
                return new String(payload, StandardCharsets.UTF_8);
            }
        }
        return null;
    }

    private String encode(final String payload, final String secret) {
        return ENCODER.encodeToString(payload.getBytes(StandardCharsets.UTF_8))
                + "." + ENCODER.encodeToString(hmac(payload, secret));
    }

    private byte[] hmac(final String payload, final String secret) {
        try {
            final Mac mac = Mac.getInstance(HMAC_ALGORITHM);
            mac.init(new SecretKeySpec(secret.getBytes(StandardCharsets.UTF_8), HMAC_ALGORITHM));
            return mac.doFinal(payload.getBytes(StandardCharsets.UTF_8));
        } catch (Exception e) {
            throw new IllegalStateException("sticky session affinity hmac failure", e);
        }
    }

    /**
     * The outcome of resolving a request's binding: the eligible bound target,
     * and whether the request carried a still-valid binding at all. The
     * distinction matters for the STRICT policy, which rejects only when a
     * valid binding exists and its target is not eligible — a request without
     * a binding is a new session and must be allocated by the base algorithm.
     */
    public static final class Resolution {

        private final Upstream bound;

        private final boolean bindingPresent;

        private Resolution(final Upstream bound, final boolean bindingPresent) {
            this.bound = bound;
            this.bindingPresent = bindingPresent;
        }

        /**
         * get bound.
         *
         * @return the bound target, or null when there is no eligible binding
         */
        public Upstream getBound() {
            return bound;
        }

        /**
         * Whether the request carried a valid, unexpired binding for this rule.
         *
         * @return true when a binding was presented (its target may still be ineligible)
         */
        public boolean isBindingPresent() {
            return bindingPresent;
        }
    }

}
