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

package org.apache.shenyu.common.graph;

import org.apache.shenyu.common.dto.GraphBranchData;
import org.apache.shenyu.common.dto.GraphNodeData;
import org.apache.shenyu.common.enums.GraphNodeTypeEnum;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Static helpers describing the topology of a plugin orchestration graph node.
 *
 * <p>Shared by the validator and the compiler so successor resolution and node-type tests are defined
 * exactly once. Successor resolution is structural only: it never evaluates branch conditions (that is
 * a request-time concern of the GRAPH executor, issue #7996).</p>
 *
 * @since 2.7.2
 */
public final class GraphTopology {

    private GraphTopology() {
    }

    /**
     * Whether the value is null or blank.
     *
     * @param value the string to test
     * @return true when null or blank
     */
    public static boolean isBlank(final String value) {
        return Objects.isNull(value) || value.isBlank();
    }

    /**
     * Whether the node is a {@code PLUGIN} node.
     *
     * @param node the node
     * @return true for a PLUGIN node
     */
    public static boolean isPlugin(final GraphNodeData node) {
        return Objects.nonNull(node) && GraphNodeTypeEnum.PLUGIN.getName().equals(node.getType());
    }

    /**
     * Whether the node is a {@code CHOICE} node.
     *
     * @param node the node
     * @return true for a CHOICE node
     */
    public static boolean isChoice(final GraphNodeData node) {
        return Objects.nonNull(node) && GraphNodeTypeEnum.CHOICE.getName().equals(node.getType());
    }

    /**
     * Whether the node is an {@code END} node.
     *
     * @param node the node
     * @return true for an END node
     */
    public static boolean isEnd(final GraphNodeData node) {
        return Objects.nonNull(node) && GraphNodeTypeEnum.END.getName().equals(node.getType());
    }

    /**
     * The structural successor node ids of a node, in evaluation order.
     *
     * <p>A {@code PLUGIN} node yields its single {@code next}; a {@code CHOICE} node yields its
     * distinct branch targets in declaration order followed by its {@code defaultTarget}; an
     * {@code END} node yields none.</p>
     *
     * @param node the node
     * @return the successor node ids (never null)
     */
    public static List<String> successorsOf(final GraphNodeData node) {
        if (isPlugin(node)) {
            return isBlank(node.getNext()) ? Collections.emptyList() : Collections.singletonList(node.getNext());
        }
        if (isChoice(node)) {
            return choiceSuccessors(node);
        }
        return Collections.emptyList();
    }

    private static List<String> choiceSuccessors(final GraphNodeData node) {
        final List<String> targets = new ArrayList<>();
        if (Objects.nonNull(node.getConditions())) {
            for (GraphBranchData branch : node.getConditions()) {
                if (Objects.nonNull(branch) && !isBlank(branch.getTarget()) && !targets.contains(branch.getTarget())) {
                    targets.add(branch.getTarget());
                }
            }
        }
        if (!isBlank(node.getDefaultTarget()) && !targets.contains(node.getDefaultTarget())) {
            targets.add(node.getDefaultTarget());
        }
        return targets;
    }
}
