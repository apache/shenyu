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


package org.apache.shenyu.web.graph;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.GraphBranchData;
import org.apache.shenyu.common.dto.GraphData;
import org.apache.shenyu.common.dto.GraphNodeData;
import org.apache.shenyu.common.enums.GraphNodeTypeEnum;
import org.apache.shenyu.common.graph.GraphCompiler;
import org.apache.shenyu.common.graph.GraphPlan;
import org.apache.shenyu.common.graph.GraphValidator;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.condition.data.ParameterDataFactory;
import org.apache.shenyu.plugin.base.condition.judge.PredicateJudgeFactory;
import org.apache.shenyu.plugin.base.condition.strategy.MatchStrategy;
import org.apache.shenyu.plugin.base.condition.strategy.MatchStrategyFactory;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Prepares a structural graph for execution outside the request path.
 * The caller supplies one snapshot of the authoritative enabled plugin catalog.
 * Full pipeline admission and resource retirement belong to the lifecycle provider.
 */
public final class GraphPlanBinder {

    /**
     * Compile and bind a definition. The caller must not mutate it during binding.
     *
     * @param graph graph definition
     * @param plugins enabled plugin instances from the existing catalog
     * @return an independent runtime topology snapshot
     */
    public BoundGraphPlan bind(final GraphData graph, final List<ShenyuPlugin> plugins) {
        Map<String, ShenyuPlugin> catalog = new LinkedHashMap<>();
        for (ShenyuPlugin plugin : plugins) {
            if (Objects.nonNull(catalog.putIfAbsent(plugin.named(), plugin))) {
                throw new IllegalArgumentException("Duplicate plugin in catalog: " + plugin.named());
            }
        }
        GraphPlan plan = new GraphCompiler(new GraphValidator(catalog.keySet())).compile(graph);
        Map<String, BoundGraphPlan.Node> nodes = new LinkedHashMap<>();
        for (String id : plan.nodeIds()) {
            GraphNodeData node = plan.getNode(id);
            GraphNodeTypeEnum type = GraphNodeTypeEnum.valueOf(node.getType());
            List<BoundGraphPlan.Branch> branches = new ArrayList<>();
            ShenyuPlugin plugin = null;
            String next = node.getNext();
            if (type == GraphNodeTypeEnum.PLUGIN) {
                plugin = Objects.requireNonNull(catalog.get(node.getPluginName()), "Missing plugin for node " + id);
            } else if (type == GraphNodeTypeEnum.CHOICE) {
                next = node.getDefaultTarget();
                for (GraphBranchData branch : node.getConditions()) {
                    branches.add(bindBranch(id, branch));
                }
            }
            nodes.put(id, new BoundGraphPlan.Node(type, plugin, next, branches));
        }
        return new BoundGraphPlan(plan.getGraphId(), plan.getNamespaceId(), graph.getDeploymentBinding(),
                plan.getRevision(), plan.getEntryNodeId(), nodes);
    }

    private BoundGraphPlan.Branch bindBranch(final String id, final GraphBranchData branch) {
        Integer mode = branch.getMatchMode();
        if (!Objects.equals(mode, 0) && !Objects.equals(mode, 1)) {
            throw new IllegalArgumentException("Invalid matchMode at node " + id + ": " + mode);
        }
        MatchStrategy strategy = MatchStrategyFactory.newInstance(mode);
        List<ConditionData> conditions = new ArrayList<>();
        for (ConditionData original : branch.getConditions()) {
            try {
                ParameterDataFactory.newInstance(original.getParamType());
                PredicateJudgeFactory.newInstance(original.getOperator());
            } catch (RuntimeException ex) {
                throw new IllegalArgumentException("Invalid condition at node " + id, ex);
            }
            ConditionData copy = new ConditionData();
            copy.setParamType(original.getParamType());
            copy.setOperator(original.getOperator());
            copy.setParamName(original.getParamName());
            copy.setParamValue(original.getParamValue());
            conditions.add(copy);
        }
        List<ConditionData> snapshot = List.copyOf(conditions);
        return new BoundGraphPlan.Branch(exchange -> strategy.match(snapshot, exchange), branch.getTarget());
    }
}
