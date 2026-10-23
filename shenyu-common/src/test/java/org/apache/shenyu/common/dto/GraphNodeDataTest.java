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

import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

/**
 * Test case for {@link GraphNodeData}.
 */
public class GraphNodeDataTest {

    @Test
    public void testGetterSetter() {
        final GraphNodeData node = new GraphNodeData();
        node.setNodeId("n0");
        node.setType("PLUGIN");
        node.setPluginName("global");
        node.setNext("n1");
        node.setConditions(Collections.singletonList(new GraphBranchData()));
        node.setDefaultTarget("n2");
        node.setRequired(true);

        assertThat(node.getNodeId(), is("n0"));
        assertThat(node.getType(), is("PLUGIN"));
        assertThat(node.getPluginName(), is("global"));
        assertThat(node.getNext(), is("n1"));
        assertThat(node.getConditions(), hasSize(1));
        assertThat(node.getDefaultTarget(), is("n2"));
        assertThat(node.isRequired(), is(true));
    }

    @Test
    public void testEqualsAndHashCode() {
        final GraphNodeData one = sample();
        final GraphNodeData two = sample();
        final Set<GraphNodeData> set = new HashSet<>();
        set.add(one);
        set.add(two);
        assertThat(set, hasSize(1));
        assertThat(one, is(two));
        assertThat(one.hashCode(), is(two.hashCode()));
    }

    @Test
    public void testRequiredAffectsEquality() {
        final GraphNodeData one = sample();
        final GraphNodeData two = sample();
        two.setRequired(true);
        assertThat(one.equals(two), is(false));
    }

    private GraphNodeData sample() {
        final GraphNodeData node = new GraphNodeData();
        node.setNodeId("n0");
        node.setType("PLUGIN");
        node.setPluginName("global");
        node.setNext("n1");
        return node;
    }
}
