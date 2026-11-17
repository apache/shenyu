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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.Flux;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiProxyCredentialFailureClassifierTest {

    private static final String ENDPOINT = "https://primary.example/v1/chat/completions";

    private final AiProxyRetryProperties.CredentialFailureFallbackProperties properties =
            new AiProxyRetryProperties.CredentialFailureFallbackProperties();

    @Test
    void defaultsAreDisabledAndRequireDeclarationsWhenEnabled() {
        assertFalse(properties.isEnabled());
        assertTrue(properties.getDeclarations().isEmpty());
        assertEquals("error.code", declaration().getType());
        assertTrue(classifier().findRules(URI.create(ENDPOINT), OpenAiChat.NAME, response(401)).isEmpty());
        properties.setEnabled(true);
        assertThrows(IllegalArgumentException.class, this::classifier);
    }

    @Test
    void snapshotsDeclarationsAndNormalizesOnlyHostAndDefaultPort() {
        AiProxyRetryProperties.CredentialFailureDeclaration rule = declaration();
        properties.setDeclarations(new ArrayList<>(List.of(rule)));
        properties.setEnabled(true);
        final AiProxyCredentialFailureClassifier classifier = classifier();
        rule.setValue("permission_denied");
        rule.setType("error.type");
        properties.getDeclarations().clear();
        properties.setEnabled(false);
        assertEquals(rules("credential_revoked"), classifier.findRules(
                URI.create("https://PRIMARY.example:443/v1/chat/completions"), OpenAiChat.NAME, response(401)));
        assertTrue(classifier.findRules(URI.create(ENDPOINT), "openai-responses", response(401)).isEmpty());
        assertTrue(classifier.findRules(URI.create(ENDPOINT), OpenAiChat.NAME, response(403)).isEmpty());
        assertTrue(classifier.findRules(URI.create(ENDPOINT), OpenAiChat.NAME, response(500)).isEmpty());
    }

    @ParameterizedTest
    @ValueSource(strings = {"https://other.example/v1/chat/completions", "https://primary.example:444/v1/chat/completions",
        "https://primary.example/v1/chat/completions/", "https://primary.example/v1/CHAT/completions",
        "https://primary.example/v1/chat/%63ompletions", "https://primary.example/v1/x/../chat/completions",
        "https://primary.example/v1/chat/completions?key=fixture", "http://primary.example/v1/chat/completions"})
    void doesNotBroadenEndpointTrust(final String endpoint) {
        enable();
        assertTrue(classifier().findRules(URI.create(endpoint), OpenAiChat.NAME, response(401)).isEmpty());
    }

    @ParameterizedTest
    @ValueSource(strings = {"http://primary.example/v1/chat/completions", "https://primary.example",
        "https://user:secret@primary.example/v1/chat/completions", "https://primary.example/v1/chat/completions?key=secret",
        "https://primary.example/v1/chat/completions#fragment", "https://*.example/v1/chat/completions",
        "https://primary.example/v1/*", "https://primary.example/v1/chat/completions*", "invalid-secret"})
    void rejectsInvalidEndpointsWithoutLeakingValues(final String endpoint) {
        invalid(rule -> rule.setEndpoint(endpoint));
    }

    @Test
    void validatesDisabledDeclarationsAndRejectsMissingOrAmbiguousConfiguration() {
        invalid(rule -> rule.setType(null));
        invalid(rule -> rule.setStatus(400));
        invalid(rule -> rule.setProtocol("unsupported-secret"));
        invalid(rule -> rule.setProtocol(null));
        invalid(rule -> rule.setValue(null));
        invalid(rule -> rule.setValue(" "));
        invalid(rule -> rule.setValue("x".repeat(129)));
        invalid(rule -> rule.setValue("code\n"));
        properties.setDeclarations(java.util.Arrays.asList((AiProxyRetryProperties.CredentialFailureDeclaration) null));
        assertThrows(IllegalArgumentException.class, this::classifier);
        properties.setDeclarations(List.of(declaration(), declaration()));
        assertThrows(IllegalArgumentException.class, this::classifier);
    }

    @ParameterizedTest
    @ValueSource(strings = {"", " ", "code", "type", "error.message", "error.CODE", "$.error.code", "error.code.extra"})
    void rejectsUnsupportedFieldPaths(final String type) {
        invalid(rule -> rule.setType(type));
    }

    @ParameterizedTest
    @ValueSource(strings = {"code", "type"})
    void readsOnlySelectedTextualField(final String field) {
        AiProxyRetryProperties.CredentialFailureDeclaration declaration = declaration();
        declaration.setType("error." + field);
        properties.setDeclarations(List.of(declaration));
        properties.setEnabled(true);
        final AiProxyCredentialFailureClassifier classifier = classifier();
        final List<AiProxyCredentialFailureClassifier.Rule> rules = classifier.findRules(URI.create(ENDPOINT), OpenAiChat.NAME, response(401));
        assertTrue(classifier.matches(("{\"error\":{\"" + field + "\":\"credential_revoked\"}}").getBytes(StandardCharsets.UTF_8), rules));
        for (String value : List.of("401", "true", "null", "{}", "[]", "\"CREDENTIAL_REVOKED\"")) {
            assertFalse(classifier.matches(("{\"error\":{\"" + field + "\":" + value + "}}").getBytes(StandardCharsets.UTF_8), rules));
        }
        final String other = "code".equals(field) ? "type" : "code";
        assertFalse(classifier.matches(("{\"error\":{\"" + other + "\":\"credential_revoked\"}}").getBytes(StandardCharsets.UTF_8), rules));
        assertFalse(classifier.matches("{\"error.type\":\"credential_revoked\",\"error\":{}}".getBytes(StandardCharsets.UTF_8), rules));
    }

    @Test
    void matchesAnyRuleInSameScopeWithoutMixingFieldsAndValues() {
        AiProxyRetryProperties.CredentialFailureDeclaration second = declaration();
        second.setValue("credential_expired");
        AiProxyRetryProperties.CredentialFailureDeclaration third = declaration();
        third.setType("error.type");
        third.setValue("authentication_failed");
        properties.setDeclarations(List.of(declaration(), second, third));
        properties.setEnabled(true);
        final AiProxyCredentialFailureClassifier classifier = classifier();
        final List<AiProxyCredentialFailureClassifier.Rule> rules = classifier.findRules(URI.create(ENDPOINT), OpenAiChat.NAME, response(401));
        assertEquals(3, rules.size());
        second.setValue("changed");
        third.setType("error.code");
        assertThrows(UnsupportedOperationException.class, rules::clear);
        for (String json : List.of("{\"error\":{\"code\":\"credential_revoked\"}}", "{\"error\":{\"code\":\"credential_expired\"}}",
                "{\"error\":{\"type\":\"authentication_failed\"}}", "{\"error\":{\"code\":\"other\",\"type\":\"authentication_failed\"}}")) {
            assertTrue(classifier.matches(json.getBytes(StandardCharsets.UTF_8), rules));
        }
        assertFalse(classifier.matches("{\"error\":{\"code\":\"authentication_failed\",\"type\":\"credential_expired\"}}".getBytes(StandardCharsets.UTF_8), rules));
        assertTrue(classifier.findRules(URI.create(ENDPOINT), OpenAiChat.NAME, response(403)).isEmpty());
    }

    @Test
    void allowsSameValueAtDifferentFieldsButRejectsNormalizedExactDuplicates() {
        AiProxyRetryProperties.CredentialFailureDeclaration second = declaration();
        second.setType("error.type");
        properties.setDeclarations(List.of(declaration(), second));
        properties.setEnabled(true);
        assertEquals(2, classifier().findRules(URI.create(ENDPOINT), OpenAiChat.NAME, response(401)).size());
        second.setType("error.code");
        second.setEndpoint("https://PRIMARY.example:443/v1/chat/completions");
        final IllegalArgumentException failure = assertThrows(IllegalArgumentException.class, this::classifier);
        assertEquals("credentialFailureFallback.invalidDeclaration[1]", failure.getMessage());
        assertNull(failure.getCause());
    }

    @ParameterizedTest
    @ValueSource(strings = {"", " ", "null", "[]", "{}", "{", "{\"error\":{\"code\":401}}",
        "{\"error\":{\"message\":\"credential_revoked\"}}", "{\"error\":{\"code\":\"permission_denied\"}}",
        "{\"error\":{\"code\":\"credential_revoked\",\"code\":\"credential_revoked\"}}",
        "{\"error\":{\"code\":\"credential_revoked\"}} {}", "{\"error\":{\"code\":\"credential_revoked\"}} trailing"})
    void unknownAndAmbiguousBodiesNeverMatch(final String body) {
        assertFalse(classifier().matches(body.getBytes(StandardCharsets.UTF_8), rules("credential_revoked")));
    }

    @Test
    void matchesOnlyExactStringInCompleteJson() {
        byte[] body = "{\"error\":{\"code\":\"credential_revoked\",\"message\":\"fixture\"}}".getBytes(StandardCharsets.UTF_8);
        assertTrue(classifier().matches(body, rules("credential_revoked")));
        assertFalse(classifier().matches(body, rules("CREDENTIAL_REVOKED")));
        assertFalse(classifier().matches(new byte[AiProxyCredentialFailureInspector.MAX_INSPECTION_BYTES + 1], rules("credential_revoked")));
    }

    @Test
    void requiresUnambiguousJsonMediaTypeAndSupportedEncoding() {
        enable();
        for (Map<String, List<String>> headers : List.of(Map.<String, List<String>>of(),
                Map.of("Content-Type", List.of("text/plain")), Map.of("Content-Type", List.of("invalid")),
                Map.of("Content-Type", List.of("application/*+json")), Map.of("Content-Type", List.of("application/*")),
                Map.of("Content-Type", List.of("*/*")),
                Map.of("Content-Type", List.of("application/json", "application/json")),
                Map.of("Content-Type", List.of("application/json"), "content-type", List.of("application/json")),
                Map.of("Content-Type", List.of("application/json"), "Content-Encoding", List.of("gzip")))) {
            assertTrue(classifier().findRules(URI.create(ENDPOINT), OpenAiChat.NAME,
                    new AiUpstreamResponse(401, headers, Flux.empty())).isEmpty());
        }
        assertFalse(classifier().findRules(URI.create(ENDPOINT), OpenAiChat.NAME,
                new AiUpstreamResponse(401, Map.of("content-type", List.of("application/problem+json; charset=UTF-8")), Flux.empty())).isEmpty());
    }

    private List<AiProxyCredentialFailureClassifier.Rule> rules(final String value) {
        return List.of(new AiProxyCredentialFailureClassifier.Rule("code", value));
    }

    private void enable() {
        properties.setDeclarations(List.of(declaration()));
        properties.setEnabled(true);
    }

    private void invalid(final Consumer<AiProxyRetryProperties.CredentialFailureDeclaration> change) {
        AiProxyRetryProperties.CredentialFailureDeclaration rule = declaration();
        change.accept(rule);
        properties.setDeclarations(List.of(rule));
        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class, this::classifier);
        assertEquals("credentialFailureFallback.invalidDeclaration[0]", failure.getMessage());
        assertNull(failure.getCause());
    }

    private AiProxyRetryProperties.CredentialFailureDeclaration declaration() {
        AiProxyRetryProperties.CredentialFailureDeclaration rule = new AiProxyRetryProperties.CredentialFailureDeclaration();
        rule.setEndpoint(ENDPOINT);
        rule.setProtocol(OpenAiChat.NAME);
        rule.setStatus(401);
        rule.setValue("credential_revoked");
        return rule;
    }

    private AiProxyCredentialFailureClassifier classifier() {
        return new AiProxyCredentialFailureClassifier(properties, new AiProxyProtocolFactory(List.of(new OpenAiChat())));
    }

    private AiUpstreamResponse response(final int status) {
        return new AiUpstreamResponse(status, Map.of("Content-Type", List.of("application/json")), Flux.empty());
    }
}
