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

package org.apache.shenyu.admin.validation.validator;

import org.apache.shenyu.common.enums.OperatorEnum;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.web.util.pattern.PatternParseException;

import java.util.regex.PatternSyntaxException;

/**
 * Test cases for {@link UriConditionValidator}.
 */
public class UriConditionValidatorTest {

    @Test
    public void testValidateErrorRegex() {
        String pattern = "[abc";
        Assertions.assertThrows(PatternSyntaxException.class,
                () -> UriConditionValidator.validate(OperatorEnum.REGEX.getAlias(), pattern));
    }

    @Test
    public void testValidateHappyRegex() {
        String pattern = "^\\/[a-zA-Z0-9\\-_\\/]+$";
        Assertions.assertDoesNotThrow(() -> UriConditionValidator.validate(OperatorEnum.REGEX.getAlias(), pattern));
    }

    @Test
    public void testValidateHappyPathPattern() {
        String pattern = "/http/**";
        Assertions.assertDoesNotThrow(() -> UriConditionValidator.validate(OperatorEnum.PATH_PATTERN.getAlias(), pattern));
    }

    @Test
    public void testValidateErrorPathPattern() {
        String pattern = "/http/{abc";
        Assertions.assertThrows(PatternParseException.class,
                () -> UriConditionValidator.validate(OperatorEnum.PATH_PATTERN.getAlias(), pattern));
    }

    @Test
    public void testValidateHappyIsBlank() {
        String pattern = "";
        Assertions.assertDoesNotThrow(() -> UriConditionValidator.validate(OperatorEnum.IS_BLANK.getAlias(), pattern));
    }

    @Test
    public void testValidateErrorIsBlank() {
        String pattern = "/http";
        Assertions.assertThrows(IllegalArgumentException.class,
                () -> UriConditionValidator.validate(OperatorEnum.IS_BLANK.getAlias(), pattern));
    }

    @Test
    public void testValidateEmptyValueForNormalOperator() {
        String pattern = "   ";
        Assertions.assertThrows(IllegalArgumentException.class,
                () -> UriConditionValidator.validate(OperatorEnum.EQ.getAlias(), pattern));
    }

    @Test
    public void testValidateHappyOtherCondition() {
        String pattern = "/http/test";
        Assertions.assertDoesNotThrow(() -> UriConditionValidator.validate(OperatorEnum.STARTS_WITH.getAlias(), pattern));
        Assertions.assertDoesNotThrow(() -> UriConditionValidator.validate(OperatorEnum.ENDS_WITH.getAlias(), pattern));
        Assertions.assertDoesNotThrow(() -> UriConditionValidator.validate(OperatorEnum.MATCH.getAlias(), pattern));
        Assertions.assertDoesNotThrow(() -> UriConditionValidator.validate(OperatorEnum.EQ.getAlias(), pattern));
        Assertions.assertDoesNotThrow(() -> UriConditionValidator.validate(OperatorEnum.EXCLUDE.getAlias(), pattern));
        Assertions.assertDoesNotThrow(() -> UriConditionValidator.validate(OperatorEnum.CONTAINS.getAlias(), pattern));
    }

    @Test
    public void testValidateErrorOtherCondition() {
        String pattern = "http/test";
        Assertions.assertThrows(IllegalArgumentException.class, () -> UriConditionValidator.validate(OperatorEnum.STARTS_WITH.getAlias(), pattern));
        Assertions.assertThrows(IllegalArgumentException.class, () -> UriConditionValidator.validate(OperatorEnum.ENDS_WITH.getAlias(), pattern));
        Assertions.assertThrows(IllegalArgumentException.class, () -> UriConditionValidator.validate(OperatorEnum.MATCH.getAlias(), pattern));
        Assertions.assertThrows(IllegalArgumentException.class, () -> UriConditionValidator.validate(OperatorEnum.EQ.getAlias(), pattern));
        Assertions.assertThrows(IllegalArgumentException.class, () -> UriConditionValidator.validate(OperatorEnum.EXCLUDE.getAlias(), pattern));
        Assertions.assertThrows(IllegalArgumentException.class, () -> UriConditionValidator.validate(OperatorEnum.CONTAINS.getAlias(), pattern));
    }
}