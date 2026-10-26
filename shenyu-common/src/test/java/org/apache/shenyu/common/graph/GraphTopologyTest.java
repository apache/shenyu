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
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;

/**
 * Test case for {@link GraphTopology}.
 */
public class GraphTopologyTest {

    @Test
    public void testIsBlank() {
        assertThat(GraphTopology.isBlank(null), is(true));
        assertThat(GraphTopology.isBlank("  "), is(true));
        assertThat(GraphTopology.isBlank("x"), is(false));
    }

    @Test
    public void testNodeTypePredicates() {
        assertThat(GraphTopology.isPlugin(node(GraphNodeTypeEnum.PLUGIN)), is(true));
        assertThat(GraphTopology.isChoice(node(GraphNodeTypeEnum.CHOICE)), is(true));
        assertThat(GraphTopology.isEnd(node(GraphNodeTypeEnum.END)), is(true));
        assertThat(GraphTopology.isPlugin(null), is(false));
        assertThat(GraphTopology.isPlugin(node(GraphNodeTypeEnum.END)), is(false));
    }

    @Test
    public void testSuccessorsOfPlugin() {
        final GraphNodeData plugin = node(GraphNodeTypeEnum.PLUGIN);
        plugin.setNext("n1");
        assertThat(GraphTopology.successorsOf(plugin), contains("n1"));

        final GraphNodeData noNext = node(GraphNodeTypeEnum.PLUGIN);
        assertThat(GraphTopology.successorsOf(noNext), is(empty()));
    }

    @Test
    public void testSuccessorsOfEnd() {
        assertThat(GraphTopology.successorsOf(node(GraphNodeTypeEnum.END)), is(empty()));
    }

    @Test
    public void testSuccessorsOfChoiceAppendsDistinctDefault() {
        final GraphNodeData choice = node(GraphNodeTypeEnum.CHOICE);
        choice.setConditions(Arrays.asList(branch("n1"), branch("n2")));
        choice.setDefaultTarget("n3");
        assertThat(GraphTopology.successorsOf(choice), contains("n1", "n2", "n3"));
    }

    @Test
    public void testSuccessorsOfChoiceDoesNotDuplicateDefault() {
        final GraphNodeData choice = node(GraphNodeTypeEnum.CHOICE);
        choice.setConditions(Collections.singletonList(branch("n1")));
        choice.setDefaultTarget("n1");
        assertThat(GraphTopology.successorsOf(choice), contains("n1"));
    }

    private GraphNodeData node(final GraphNodeTypeEnum type) {
        final GraphNodeData node = new GraphNodeData();
        node.setNodeId("id");
        node.setType(type.getName());
        return node;
    }

    private GraphBranchData branch(final String target) {
        return new GraphBranchData(Collections.emptyList(), 0, target);
    }
}
