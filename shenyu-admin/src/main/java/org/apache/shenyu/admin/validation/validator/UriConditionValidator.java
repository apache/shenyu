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

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.springframework.web.util.pattern.PathPatternParser;

public class UriConditionValidator {

    private static final Map<String, Consumer<String>> VALIDATOR_MAP = new HashMap<>();

    static {
        VALIDATOR_MAP.put(OperatorEnum.PATH_PATTERN.getAlias(),
                PathPatternParser.defaultInstance::parse);
        VALIDATOR_MAP.put(OperatorEnum.REGEX.getAlias(), Pattern::compile);

        Consumer<String> commonPathValidator = value -> {
            if (!value.startsWith("/")) {
                throw new IllegalArgumentException("The URI must start with '/'");
            }
            if (StringUtils.containsAny(value, " ", "\t", "\n")) {
                throw new IllegalArgumentException(
                        "The URI cannot contain whitespaces. Current value: " + value);
            }
        };
        Consumer<String> blankPathValidator = value -> {
            if (StringUtils.isNotBlank(value)) {
                throw new IllegalArgumentException("The URI must be blank");
            }
        };
        VALIDATOR_MAP.put(OperatorEnum.EQ.getAlias(), commonPathValidator);
        VALIDATOR_MAP.put(OperatorEnum.STARTS_WITH.getAlias(), commonPathValidator);
        VALIDATOR_MAP.put(OperatorEnum.ENDS_WITH.getAlias(), commonPathValidator);
        VALIDATOR_MAP.put(OperatorEnum.MATCH.getAlias(), commonPathValidator);
        VALIDATOR_MAP.put(OperatorEnum.EXCLUDE.getAlias(), commonPathValidator);
        VALIDATOR_MAP.put(OperatorEnum.CONTAINS.getAlias(), commonPathValidator);
        VALIDATOR_MAP.put(OperatorEnum.IS_BLANK.getAlias(), blankPathValidator);
    }

    public static void validate(final String operator, final String value) {
        if (!OperatorEnum.IS_BLANK.getAlias().equals(operator) && StringUtils.isBlank(value)) {
            throw new IllegalArgumentException("The URI condition value cannot be empty.");
        }
        Consumer<String> validator = VALIDATOR_MAP.get(operator);
        if (Objects.nonNull(validator)) {
            validator.accept(value);
        }
    }

}
