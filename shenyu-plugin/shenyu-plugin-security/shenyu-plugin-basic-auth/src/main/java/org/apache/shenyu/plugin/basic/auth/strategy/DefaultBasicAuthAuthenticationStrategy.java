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

package org.apache.shenyu.plugin.basic.auth.strategy;

import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.basic.auth.rule.BasicAuthRuleHandle;
import org.apache.shenyu.plugin.basic.auth.rule.DefaultBasicAuthRuleHandle;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Base64;
import java.util.Objects;

/**
 * Default authentication strategy for RFC 7617 Basic credentials and legacy literal credentials.
 *
 * <p>The Basic scheme is matched case-insensitively and its decoded value is compared in constant time. Exact
 * literal matching is retained for compatibility, including configurations containing a complete Basic header.
 * The URI user-info fallback is selected by {@link org.apache.shenyu.plugin.basic.auth.BasicAuthPlugin} before this
 * strategy is invoked.</p>
 */
@Join
public class DefaultBasicAuthAuthenticationStrategy implements BasicAuthAuthenticationStrategy {

    private static final String BASIC_SCHEME = "Basic";

    private static final Logger LOG = LoggerFactory.getLogger(DefaultBasicAuthAuthenticationStrategy.class);

    @Override
    public DefaultBasicAuthRuleHandle parseHandleJson(final String handleJson) {
        try {
            return GsonUtils.getInstance().fromJson(handleJson, DefaultBasicAuthRuleHandle.class);
        } catch (Exception exception) {
            LOG.error("Failed to parse json , please check json format", exception);
            return null;
        }
    }

    /**
     * Authenticate an RFC 7617 Basic credential or a legacy literal credential.
     *
     * <p>The Basic scheme is case-insensitive and permits one or more spaces before the Base64 token. Valid Basic
     * credentials are decoded as UTF-8 and compared with the configured authorization in constant time. An exact,
     * constant-time literal comparison is also retained for compatibility with configurations containing the full
     * Basic header. Malformed Basic credentials fail without falling back to literal matching. Non-Basic values use
     * the legacy exact literal comparison. URI user-info fallback is handled by
     * {@link org.apache.shenyu.plugin.basic.auth.BasicAuthPlugin}.</p>
     *
     * @param basicAuthRuleHandle basic auth rule handle
     * @param authentication supplied authorization value
     * @return true when the supplied authorization matches the configured value
     */
    @Override
    public boolean authenticate(final BasicAuthRuleHandle basicAuthRuleHandle, final String authentication) {
        if (!(basicAuthRuleHandle instanceof DefaultBasicAuthRuleHandle) || Objects.isNull(authentication)) {
            return false;
        }
        String configuredAuthorization = ((DefaultBasicAuthRuleHandle) basicAuthRuleHandle).getAuthorization();
        if (Objects.isNull(configuredAuthorization)) {
            return false;
        }
        String trimmedAuthentication = authentication.trim();
        if (!hasBasicScheme(trimmedAuthentication)) {
            return constantTimeEquals(authentication, configuredAuthorization);
        }
        String token = extractBasicToken(trimmedAuthentication);
        String decodedCredential = decodeBasicToken(token);
        if (Objects.isNull(decodedCredential)) {
            return false;
        }
        boolean decodedMatches = constantTimeEquals(decodedCredential, configuredAuthorization);
        boolean literalMatches = constantTimeEquals(authentication, configuredAuthorization);
        return decodedMatches || literalMatches;
    }

    private boolean hasBasicScheme(final String authentication) {
        if (!authentication.regionMatches(true, 0, BASIC_SCHEME, 0, BASIC_SCHEME.length())) {
            return false;
        }
        return authentication.length() == BASIC_SCHEME.length()
                || authentication.charAt(BASIC_SCHEME.length()) == ' ';
    }

    private String extractBasicToken(final String authentication) {
        int tokenStart = BASIC_SCHEME.length();
        while (tokenStart < authentication.length() && authentication.charAt(tokenStart) == ' ') {
            tokenStart++;
        }
        return authentication.substring(tokenStart);
    }

    private String decodeBasicToken(final String token) {
        if (token.isEmpty()) {
            return null;
        }
        try {
            byte[] decodedToken = Base64.getDecoder().decode(token);
            return StandardCharsets.UTF_8.newDecoder()
                    .onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT)
                    .decode(ByteBuffer.wrap(decodedToken))
                    .toString();
        } catch (IllegalArgumentException | CharacterCodingException ignored) {
            return null;
        }
    }

    private boolean constantTimeEquals(final String supplied, final String configured) {
        return MessageDigest.isEqual(supplied.getBytes(StandardCharsets.UTF_8), configured.getBytes(StandardCharsets.UTF_8));
    }
}
