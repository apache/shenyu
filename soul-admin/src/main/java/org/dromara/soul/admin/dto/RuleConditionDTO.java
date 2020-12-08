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

package org.dromara.soul.admin.dto;

import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * this is rule condition from by web front.
 *
 * @author jiangxiaofeng(Nicholas)
 * @author nuo-promise
 */
@Data
@NoArgsConstructor
public final class RuleConditionDTO implements Serializable {

    /**
     * primary key.
     */
    private String id;

    /**
     * rule id.
     */
    private String ruleId;

    /**
     * parameter type.
     */
    private String paramType;

    /**
     * match operator.
     */
    private String operator;

    /**
     * parameter name.
     */
    private String paramName;

    /**
     * parameter value.
     */
    private String paramValue;

    @Builder
    private RuleConditionDTO(final String id, final String ruleId, final String paramType, final String operator,
                             final String paramName, final String paramValue) {
        this.id = id;
        this.ruleId = ruleId;
        this.paramType = paramType;
        this.operator = operator;
        this.paramName = paramName;
        this.paramValue = paramValue;
    }
}
