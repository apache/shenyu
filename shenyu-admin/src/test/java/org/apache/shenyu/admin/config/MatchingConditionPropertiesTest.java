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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.validation.MatchingConditionEnforcement;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests matching-condition property binding and enforcement lifecycle isolation.
 */
final class MatchingConditionPropertiesTest {

    private final ApplicationContextRunner contextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ConfigurationPropertiesAutoConfiguration.class))
            .withUserConfiguration(MatchingConditionProperties.class);

    @AfterEach
    void resetStrictValidation() {
        MatchingConditionEnforcement.setStrictValidation(false);
    }

    @Test
    void shouldApplyDefaultAndRestorePreviousValue() {
        MatchingConditionEnforcement.setStrictValidation(true);
        boolean previousStrictValidation = MatchingConditionEnforcement.isStrictValidation();

        contextRunner.run(context -> {
            assertFalse(context.getBean(MatchingConditionProperties.class).isStrictValidation());
            assertFalse(MatchingConditionEnforcement.isStrictValidation());
        });

        assertEquals(previousStrictValidation, MatchingConditionEnforcement.isStrictValidation());
    }

    @Test
    void shouldApplyStrictPropertyAndRestorePreviousValue() {
        boolean previousStrictValidation = MatchingConditionEnforcement.isStrictValidation();

        contextRunner.withPropertyValues("shenyu.matching-condition.strict-validation=true").run(context -> {
            assertTrue(context.getBean(MatchingConditionProperties.class).isStrictValidation());
            assertTrue(MatchingConditionEnforcement.isStrictValidation());
        });

        assertEquals(previousStrictValidation, MatchingConditionEnforcement.isStrictValidation());
    }
}
