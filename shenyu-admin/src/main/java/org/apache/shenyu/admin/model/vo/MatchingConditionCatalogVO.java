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

import java.util.List;
import java.util.Objects;

/**
 * Versioned matching-condition catalog view.
 *
 * @param version catalog version
 * @param sources source descriptors
 * @param operators operator descriptors
 */
public record MatchingConditionCatalogVO(String version, List<MatchingConditionSourceVO> sources,
                                         List<MatchingConditionOperatorVO> operators) {

    /**
     * Creates an immutable catalog view.
     */
    public MatchingConditionCatalogVO {
        Objects.requireNonNull(version, "version");
        Objects.requireNonNull(sources, "sources");
        Objects.requireNonNull(operators, "operators");
        sources = List.copyOf(sources);
        operators = List.copyOf(operators);
    }
}
