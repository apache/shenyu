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

package org.apache.shenyu.common.dto;

import java.util.List;
import java.util.Objects;

/**
 * A single ordered branch of a {@code CHOICE} graph node.
 *
 * <p>A branch reuses the existing {@link ConditionData} model so operators author conditions exactly
 * as they do for selectors/rules. Selection is deterministic: branches are evaluated in declaration
 * order and the first branch whose conditions match (under {@code matchMode}) wins; when no branch
 * matches the owning node's {@code defaultTarget} is used. See {@code docs/plugin-graph-orchestration.md}
 * (issue #7994) sections 4 and 6.</p>
 *
 * @since 2.7.2
 */
public class GraphBranchData {

    /**
     * the branch conditions, evaluated under {@code matchMode}.
     */
    private List<ConditionData> conditions;

    /**
     * match mode of {@code conditions}.
     * {@linkplain org.apache.shenyu.common.enums.MatchModeEnum} (0 and, 1 or).
     */
    private Integer matchMode;

    /**
     * the target node id selected when this branch matches.
     */
    private String target;

    /**
     * no args constructor.
     */
    public GraphBranchData() {
    }

    /**
     * all args constructor.
     *
     * @param conditions the branch conditions
     * @param matchMode  the condition match mode
     * @param target     the target node id
     */
    public GraphBranchData(final List<ConditionData> conditions, final Integer matchMode, final String target) {
        this.conditions = conditions;
        this.matchMode = matchMode;
        this.target = target;
    }

    /**
     * get conditions.
     *
     * @return conditions
     */
    public List<ConditionData> getConditions() {
        return conditions;
    }

    /**
     * set conditions.
     *
     * @param conditions conditions
     */
    public void setConditions(final List<ConditionData> conditions) {
        this.conditions = conditions;
    }

    /**
     * get matchMode.
     *
     * @return matchMode
     */
    public Integer getMatchMode() {
        return matchMode;
    }

    /**
     * set matchMode.
     *
     * @param matchMode matchMode
     */
    public void setMatchMode(final Integer matchMode) {
        this.matchMode = matchMode;
    }

    /**
     * get target.
     *
     * @return target
     */
    public String getTarget() {
        return target;
    }

    /**
     * set target.
     *
     * @param target target
     */
    public void setTarget(final String target) {
        this.target = target;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        GraphBranchData that = (GraphBranchData) o;
        return Objects.equals(conditions, that.conditions) && Objects.equals(matchMode, that.matchMode)
                && Objects.equals(target, that.target);
    }

    @Override
    public int hashCode() {
        return Objects.hash(conditions, matchMode, target);
    }

    @Override
    public String toString() {
        return "GraphBranchData{"
                + "conditions="
                + conditions
                + ", matchMode="
                + matchMode
                + ", target='"
                + target
                + '\''
                + '}';
    }
}
