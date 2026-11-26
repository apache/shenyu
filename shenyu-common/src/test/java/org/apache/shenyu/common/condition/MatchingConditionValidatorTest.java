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

package org.apache.shenyu.common.condition;

import com.google.gson.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;

import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for {@link MatchingConditionValidator} and the shared fixture corpus.
 */
final class MatchingConditionValidatorTest {

    private static final int MAX_DIAGNOSTICS = 16;

    private static final FixtureCorpus CORPUS = loadCorpus();

    @Test
    void shouldValidateStructuralCorpus() {
        assertEquals("1", CORPUS.version());
        for (StructuralFixture fixture : CORPUS.structural()) {
            MatchingConditionValidation validation = MatchingConditionValidator.validate(List.of(fixture.candidate()));
            assertEquals(MatchingConditionValidation.Status.valueOf(fixture.status()), validation.status(), fixture.id());
            if (StringUtils.isNotBlank(fixture.code())) {
                MatchingConditionDiagnostic.Code expected = MatchingConditionDiagnostic.Code.valueOf(fixture.code());
                assertTrue(validation.diagnostics().stream().anyMatch(item -> expected == item.code()), fixture.id());
            }
        }
    }

    @Test
    void shouldBoundDiagnosticsAndPreferInvalidStatus() {
        List<MatchingConditionCandidate> candidates = IntStream.range(0, MAX_DIAGNOSTICS + 2)
                .mapToObj(index -> new MatchingConditionCandidate("custom-" + index, "=", "name", "value"))
                .collect(java.util.stream.Collectors.toList());
        candidates.set(candidates.size() - 1, new MatchingConditionCandidate("", "=", "name", "value"));

        MatchingConditionValidation validation = MatchingConditionValidator.validate(candidates);

        assertEquals(MAX_DIAGNOSTICS, validation.diagnostics().size());
        assertEquals(MatchingConditionValidation.Status.INVALID, validation.status());
        assertTrue(validation.isInvalid());
        assertThrows(UnsupportedOperationException.class, () -> validation.diagnostics().clear());
    }

    @Test
    void shouldUseDeclaredConditionPaths() {
        MatchingConditionValidation validation = MatchingConditionValidator.validate(List.of(
                new MatchingConditionCandidate("uri", "=", "", "")));

        assertEquals("conditions[0].paramValue", validation.diagnostics().get(0).path());
        assertTrue(validation.diagnostics().get(0).path().matches("[A-Za-z0-9_.\\[\\]]{1,128}"));
        assertThrows(IllegalArgumentException.class, () -> new MatchingConditionDiagnostic(
                "conditions[0].param-Value", MatchingConditionDiagnostic.Code.VALUE_REQUIRED,
                MatchingConditionDiagnostic.Severity.INVALID));
    }

    @Test
    void shouldNotLeakConfiguredDataIntoDiagnostics() {
        MatchingConditionValidation validation = MatchingConditionValidator.validate(List.of(
                new MatchingConditionCandidate("header", "regex", "SECRET_HEADER", "[SECRET_VALUE"),
                new MatchingConditionCandidate("SECRET_SOURCE", "=", "SECRET_COOKIE", "SECRET_VALUE")));

        assertEquals(MatchingConditionValidation.Status.INVALID, validation.status());
        assertFalse(validation.toString().contains("SECRET"));
        validation.diagnostics().forEach(diagnostic -> {
            assertFalse(diagnostic.path().contains("SECRET"));
            assertFalse(diagnostic.code().name().contains("SECRET"));
        });
    }

    @Test
    void shouldProvideCompleteSemanticsCorpusForGatewayTests() {
        assertFalse(CORPUS.semantics().isEmpty());
        CORPUS.semantics().forEach(fixture -> {
            assertTrue(fixture.has("id"));
            assertTrue(fixture.has("request"));
            assertTrue(fixture.has("expected"));
        });
    }

    private static FixtureCorpus loadCorpus() {
        try (InputStreamReader reader = new InputStreamReader(Objects.requireNonNull(
                MatchingConditionValidatorTest.class.getResourceAsStream("/matching-condition/corpus-v1.json")),
                StandardCharsets.UTF_8)) {
            return GsonUtils.getGson().fromJson(reader, FixtureCorpus.class);
        } catch (java.io.IOException ex) {
            throw new IllegalStateException("Unable to load matching-condition fixture corpus", ex);
        }
    }

    private record FixtureCorpus(String version, List<StructuralFixture> structural, List<JsonObject> semantics) {
    }

    private record StructuralFixture(String id, String source, String operator, String paramName,
                                     String paramValue, String status, String code) {

        private MatchingConditionCandidate candidate() {
            return new MatchingConditionCandidate(source, operator, paramName, paramValue);
        }
    }
}
