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

package org.apache.shenyu.plugin.agent.gateway.delegation;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig.IdentityDelegation;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig.Tool;
import org.apache.shenyu.plugin.agent.gateway.delegation.AgentIdentityDelegationException.Reason;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.security.KeyFactory;
import java.security.spec.PKCS8EncodedKeySpec;
import java.time.Instant;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

/** Resolves a versioned, operator-mounted PKCS#8 key for a single authorized HTTP route. */
public final class FileDelegationSigningKeyResolver implements DelegationSigningKeyResolver {

    private static final ObjectMapper JSON = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION).enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final Pattern SCOPE = Pattern.compile("[A-Za-z0-9_.:/-]{1,64}");

    private static final int MAX_FILE_BYTES = 8192;

    private static final String EXPIRES_AT = "expiresAt";

    private static final String REVOKED = "revoked";

    private static final Set<String> FIELDS = Set.of("issuer", "credentialRef", "credentialVersion", "tool", "method", "path", "audience",
            "allowedScopes", "privateKeyPkcs8Base64", REVOKED, EXPIRES_AT);

    private final Path directory;

    public FileDelegationSigningKeyResolver(final Path directory) {
        try {
            this.directory = directory.toRealPath();
            if (!Files.isDirectory(this.directory)) {
                throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
            }
        } catch (IOException | SecurityException error) {
            throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
        }
    }

    @Override
    public SigningKey resolve(final DelegationRequest request) {
        try {
            JsonNode file = readFile(request.getPolicy());
            if (!file.isObject()) {
                throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
            }
            file.fieldNames().forEachRemaining(field -> {
                if (!FIELDS.contains(field)) {
                    throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
                }
            });
            final Instant expiresAt = verifyStatus(file);
            verifyTarget(file, request);
            String issuer = text(file, "issuer");
            byte[] bytes = Base64.getDecoder().decode(text(file, "privateKeyPkcs8Base64"));
            try {
                return new SigningKey(issuer, KeyFactory.getInstance("Ed25519").generatePrivate(new PKCS8EncodedKeySpec(bytes)), expiresAt);
            } finally {
                Arrays.fill(bytes, (byte) 0);
            }
        } catch (AgentIdentityDelegationException error) {
            throw error;
        } catch (Exception error) {
            // Filesystem, parser and crypto exceptions can contain paths or key material.
            throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
        }
    }

    private JsonNode readFile(final IdentityDelegation policy) throws IOException {
        Path file = directory.resolve(policy.getCredentialRef()).resolve(policy.getCredentialVersion() + ".json").normalize();
        if (!file.startsWith(directory)) {
            throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
        }
        Path realFile = file.toRealPath();
        if (!realFile.startsWith(directory) || !Files.isRegularFile(realFile, LinkOption.NOFOLLOW_LINKS)) {
            throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
        }
        try (InputStream input = Files.newInputStream(realFile, LinkOption.NOFOLLOW_LINKS)) {
            byte[] bytes = input.readNBytes(MAX_FILE_BYTES + 1);
            try {
                if (bytes.length > MAX_FILE_BYTES) {
                    throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
                }
                return JSON.readTree(bytes);
            } finally {
                Arrays.fill(bytes, (byte) 0);
            }
        }
    }

    private static Instant verifyStatus(final JsonNode file) {
        if (file.has(REVOKED) && !file.get(REVOKED).isBoolean()) {
            throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
        }
        Instant expiresAt = file.has(EXPIRES_AT) ? Instant.parse(text(file, EXPIRES_AT)) : Instant.MAX;
        if (file.path(REVOKED).booleanValue() || !expiresAt.isAfter(Instant.now())) {
            throw new AgentIdentityDelegationException(Reason.REVOKED_OR_EXPIRED);
        }
        return expiresAt;
    }

    private static void verifyTarget(final JsonNode file, final DelegationRequest request) {
        IdentityDelegation policy = request.getPolicy();
        match(file, "credentialRef", policy.getCredentialRef());
        match(file, "credentialVersion", policy.getCredentialVersion());
        match(file, "audience", policy.getAudience());
        Tool tool = request.getTool();
        match(file, "tool", tool.getName());
        match(file, "method", tool.getMethod());
        match(file, "path", tool.getPath());
        JsonNode scopes = file.path("allowedScopes");
        if (!scopes.isArray() || scopes.isEmpty()) {
            throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
        }
        Set<String> allowed = new HashSet<>();
        for (JsonNode scope : scopes) {
            if (!scope.isTextual() || !SCOPE.matcher(scope.textValue()).matches() || !allowed.add(scope.textValue())) {
                throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
            }
        }
        if (!allowed.containsAll(policy.getScopes())) {
            throw new AgentIdentityDelegationException(Reason.TARGET_MISMATCH);
        }
    }

    private static void match(final JsonNode file, final String field, final String expected) {
        if (!expected.equals(text(file, field))) {
            throw new AgentIdentityDelegationException(Reason.TARGET_MISMATCH);
        }
    }

    private static String text(final JsonNode file, final String field) {
        JsonNode value = file.path(field);
        if (!value.isTextual() || value.textValue().isBlank()) {
            throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
        }
        return value.textValue();
    }
}
