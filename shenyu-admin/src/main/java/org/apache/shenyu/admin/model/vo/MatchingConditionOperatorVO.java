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
import org.apache.shenyu.common.condition.MatchingConditionValueFormat;

import java.util.Objects;

/**
 * Matching-condition operator view.
 *
 * @param id operator identifier
 * @param displayKey localization key for the display name
 * @param helpKey localization key for runtime semantics
 * @param status availability and compatibility status
 * @param requiresValue whether the operator uses a configured value
 * @param valueFormat expected configured-value format
 */
public record MatchingConditionOperatorVO(String id, String displayKey, String helpKey,
                                          MatchingConditionStatus status, boolean requiresValue,
                                          MatchingConditionValueFormat valueFormat) {

    /**
     * Creates an operator view with non-null components.
     */
    public MatchingConditionOperatorVO {
        Objects.requireNonNull(id, "id");
        Objects.requireNonNull(displayKey, "displayKey");
        Objects.requireNonNull(helpKey, "helpKey");
        Objects.requireNonNull(status, "status");
        Objects.requireNonNull(valueFormat, "valueFormat");
    }
}
