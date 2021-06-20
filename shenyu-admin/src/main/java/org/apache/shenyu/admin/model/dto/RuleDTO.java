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

package org.apache.shenyu.admin.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * this is rule from by web front.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public final class RuleDTO implements Serializable {

    private static final long serialVersionUID = 995629439944393704L;

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    @NotNull
    private String selectorId;

    /**
     * match mode.
     */
    @NotNull
    private Integer matchMode;

    /**
     * rule name.
     */
    @NotNull
    private String name;

    /**
     * whether enabled.
     */
    @NotNull
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * sort type.
     */
    @NotNull
    private Integer sort;

    /**
     * process logic.
     */
    private String handle;

    /**
     * rule conditions.
     */
    @Valid
    private List<RuleConditionDTO> ruleConditions;
}
