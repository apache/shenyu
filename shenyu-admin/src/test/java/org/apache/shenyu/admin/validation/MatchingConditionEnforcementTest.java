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

package org.apache.shenyu.admin.validation;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.common.condition.MatchingConditionValidation;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests Admin enforcement against the shared structural matching-condition corpus.
 */
final class MatchingConditionEnforcementTest {

    private static final FixtureCorpus CORPUS = loadCorpus();

    @AfterEach
    void resetStrictValidation() {
        MatchingConditionEnforcement.setStrictValidation(false);
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("structuralCases")
    void shouldApplyCompatibilityPolicy(final String caseId, final StructuralFixture fixture) {
        List<SelectorConditionDTO> selectors = List.of(fixture.selectorCondition());
        List<RuleConditionDTO> rules = List.of(fixture.ruleCondition());
        MatchingConditionEnforcement.setStrictValidation(false);
        assertDoesNotThrow(() -> MatchingConditionEnforcement.checkSelectorConditions(selectors), caseId);
        assertDoesNotThrow(() -> MatchingConditionEnforcement.checkRuleConditions(rules), caseId);

        MatchingConditionEnforcement.setStrictValidation(true);
        if (MatchingConditionValidation.Status.INVALID.name().equals(fixture.status())) {
            assertDiagnostic(assertThrows(ShenyuAdminException.class,
                    () -> MatchingConditionEnforcement.checkSelectorConditions(selectors)), fixture);
            assertDiagnostic(assertThrows(ShenyuAdminException.class,
                    () -> MatchingConditionEnforcement.checkRuleConditions(rules)), fixture);
        } else {
            assertDoesNotThrow(() -> MatchingConditionEnforcement.checkSelectorConditions(selectors), caseId);
            assertDoesNotThrow(() -> MatchingConditionEnforcement.checkRuleConditions(rules), caseId);
        }
    }

    @Test
    void shouldIgnoreNullAndEmptyListsInBothModes() {
        assertEmptyInputs();
        MatchingConditionEnforcement.setStrictValidation(true);
        assertEmptyInputs();
    }

    @Test
    void shouldReportOneBoundedWarningWithoutConfiguredData() {
        SelectorConditionDTO condition = new SelectorConditionDTO();
        condition.setParamType("header");
        condition.setOperator("regex");
        condition.setParamName("X-Sensitive-Tenant");
        condition.setParamValue("[sensitive-parser-input");
        Logger logger = (Logger) LoggerFactory.getLogger(MatchingConditionEnforcement.class);
        ListAppender<ILoggingEvent> captured = new ListAppender<>();
        captured.start();
        logger.addAppender(captured);
        try {
            MatchingConditionEnforcement.checkSelectorConditions(Collections.nCopies(5, condition));

            assertEquals(1, captured.list.size());
            assertEquals(Level.WARN, captured.list.get(0).getLevel());
            String message = captured.list.get(0).getFormattedMessage();
            assertEquals("Matching condition structural issues detected (report-only, not rejected): "
                    + "conditions[0].paramValue=INVALID_REGEX, conditions[1].paramValue=INVALID_REGEX, "
                    + "conditions[2].paramValue=INVALID_REGEX, conditions[3].paramValue=INVALID_REGEX, ...", message);
            assertFalse(message.contains(condition.getParamName()));
            assertFalse(message.contains(condition.getParamValue()));
        } finally {
            logger.detachAppender(captured);
            captured.stop();
        }
    }

    private static void assertEmptyInputs() {
        assertDoesNotThrow(() -> MatchingConditionEnforcement.checkSelectorConditions(null));
        assertDoesNotThrow(() -> MatchingConditionEnforcement.checkSelectorConditions(Collections.emptyList()));
        assertDoesNotThrow(() -> MatchingConditionEnforcement.checkRuleConditions(null));
        assertDoesNotThrow(() -> MatchingConditionEnforcement.checkRuleConditions(Collections.emptyList()));
    }

    private static void assertDiagnostic(final ShenyuAdminException exception, final StructuralFixture fixture) {
        String message = exception.getMessage();
        assertTrue(message.contains("conditions[0]"), fixture.id());
        assertTrue(message.contains(fixture.code()), fixture.id());
        if (StringUtils.isNotBlank(fixture.paramName())) {
            assertFalse(message.contains(fixture.paramName()), fixture.id());
        }
        if (StringUtils.isNotBlank(fixture.paramValue())) {
            assertFalse(message.contains(fixture.paramValue()), fixture.id());
        }
    }

    private static Stream<Arguments> structuralCases() {
        return CORPUS.structural().stream().map(fixture -> Arguments.of(fixture.id(), fixture));
    }

    private static FixtureCorpus loadCorpus() {
        try (InputStreamReader reader = new InputStreamReader(Objects.requireNonNull(
                MatchingConditionEnforcementTest.class.getResourceAsStream("/matching-condition/corpus-v1.json")),
                StandardCharsets.UTF_8)) {
            return GsonUtils.getGson().fromJson(reader, FixtureCorpus.class);
        } catch (IOException ex) {
            throw new IllegalStateException("Unable to load matching-condition fixture corpus", ex);
        }
    }

    private record FixtureCorpus(String version, List<StructuralFixture> structural) {
    }

    private record StructuralFixture(String id, String source, String operator, String paramName,
                                     String paramValue, String status, String code) {

        private SelectorConditionDTO selectorCondition() {
            SelectorConditionDTO condition = new SelectorConditionDTO();
            condition.setParamType(source);
            condition.setOperator(operator);
            condition.setParamName(paramName);
            condition.setParamValue(paramValue);
            return condition;
        }

        private RuleConditionDTO ruleCondition() {
            return RuleConditionDTO.builder().paramType(source).operator(operator)
                    .paramName(paramName).paramValue(paramValue).build();
        }
    }
}
