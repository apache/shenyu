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

package org.apache.shenyu.admin.model.vo;

import org.apache.shenyu.common.condition.MatchingConditionStatus;
import org.apache.shenyu.common.condition.MatchingConditionValueType;

import java.util.List;
import java.util.Objects;

/**
 * Matching-condition source view.
 *
 * @param id source identifier
 * @param displayKey localization key for the display name
 * @param helpKey localization key for runtime semantics and availability
 * @param status availability and compatibility status
 * @param valueType runtime value category
 * @param paramNameRequired whether the source needs a parameter name
 * @param runtimeRegistered whether the built-in gateway registers the source
 * @param operatorIds applicable operator identifiers
 * @param examples source examples
 */
public record MatchingConditionSourceVO(String id, String displayKey, String helpKey,
                                        MatchingConditionStatus status, MatchingConditionValueType valueType,
                                        boolean paramNameRequired, boolean runtimeRegistered,
                                        List<String> operatorIds, List<MatchingConditionExampleVO> examples) {

    /**
     * Creates an immutable source view.
     */
    public MatchingConditionSourceVO {
        Objects.requireNonNull(id, "id");
        Objects.requireNonNull(displayKey, "displayKey");
        Objects.requireNonNull(helpKey, "helpKey");
        Objects.requireNonNull(status, "status");
        Objects.requireNonNull(valueType, "valueType");
        Objects.requireNonNull(operatorIds, "operatorIds");
        Objects.requireNonNull(examples, "examples");
        operatorIds = List.copyOf(operatorIds);
        examples = List.copyOf(examples);
    }
}
