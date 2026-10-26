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
 * Test case for {@link GraphData}.
 */
public class GraphDataTest {

    @Test
    public void testGetterSetter() {
        final GraphData graph = new GraphData();
        graph.setSchemaVersion(1);
        graph.setNamespaceId("ns");
        graph.setDeploymentBinding("deploy");
        graph.setGraphId("g");
        graph.setRevision(9L);
        graph.setContentHash("hash");
        graph.setEntryNodeId("n0");
        graph.setNodes(Collections.singletonList(new GraphNodeData()));

        assertThat(graph.getSchemaVersion(), is(1));
        assertThat(graph.getNamespaceId(), is("ns"));
        assertThat(graph.getDeploymentBinding(), is("deploy"));
        assertThat(graph.getGraphId(), is("g"));
        assertThat(graph.getRevision(), is(9L));
        assertThat(graph.getContentHash(), is("hash"));
        assertThat(graph.getEntryNodeId(), is("n0"));
        assertThat(graph.getNodes(), hasSize(1));
    }

    @Test
    public void testEqualsAndHashCode() {
        final GraphData one = sample();
        final GraphData two = sample();
        final Set<GraphData> set = new HashSet<>();
        set.add(one);
        set.add(two);
        assertThat(set, hasSize(1));
        assertThat(one, is(two));
        assertThat(one.hashCode(), is(two.hashCode()));
    }

    @Test
    public void testRevisionAffectsEquality() {
        final GraphData one = sample();
        final GraphData two = sample();
        two.setRevision(2L);
        assertThat(one.equals(two), is(false));
    }

    private GraphData sample() {
        final GraphData graph = new GraphData();
        graph.setSchemaVersion(1);
        graph.setNamespaceId("ns");
        graph.setGraphId("g");
        graph.setRevision(1L);
        graph.setContentHash("hash");
        graph.setEntryNodeId("n0");
        graph.setNodes(Collections.emptyList());
        return graph;
    }
}
