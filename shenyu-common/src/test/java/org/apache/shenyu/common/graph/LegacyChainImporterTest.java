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

import org.apache.shenyu.common.dto.GraphData;
import org.apache.shenyu.common.dto.GraphNodeData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.GraphNodeTypeEnum;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;

/**
 * Test case for {@link LegacyChainImporter}.
 */
public class LegacyChainImporterTest {

    private static final Set<String> PLUGINS = Set.of("global", "sign", "divide", "response");

    private final GraphValidator validator = new GraphValidator(PLUGINS);

    @Test
    public void testImportReproducesLegacySortOrder() {
        final List<PluginData> plugins = Arrays.asList(
                plugin("divide", 200, true),
                plugin("global", -1, true),
                plugin("response", 420, true),
                plugin("sign", 20, true));
        final GraphData graph = LegacyChainImporter.importLinearChain(plugins, "ns-test", "g-import", 7L);

        assertThat(graph.getSchemaVersion(), is(GraphValidator.SUPPORTED_SCHEMA_VERSION));
        assertThat(graph.getNamespaceId(), is("ns-test"));
        assertThat(graph.getGraphId(), is("g-import"));
        assertThat(graph.getRevision(), is(7L));
        assertThat(graph.getEntryNodeId(), is("n0"));

        final List<GraphNodeData> nodes = graph.getNodes();
        assertThat(nodes.size(), is(5));
        assertThat(nodes.get(0).getPluginName(), is("global"));
        assertThat(nodes.get(0).isRequired(), is(true));
        assertThat(nodes.get(0).getNext(), is("n1"));
        assertThat(nodes.get(1).getPluginName(), is("sign"));
        assertThat(nodes.get(1).isRequired(), is(true));
        assertThat(nodes.get(2).getPluginName(), is("divide"));
        assertThat(nodes.get(2).isRequired(), is(false));
        assertThat(nodes.get(3).getPluginName(), is("response"));
        assertThat(nodes.get(3).getNext(), is(LegacyChainImporter.END_NODE_ID));
        assertThat(nodes.get(4).getType(), is(GraphNodeTypeEnum.END.getName()));
    }

    @Test
    public void testImportSkipsDisabledPlugins() {
        final List<PluginData> plugins = Arrays.asList(
                plugin("global", -1, true),
                plugin("sign", 20, false),
                plugin("divide", 200, true));
        final GraphData graph = LegacyChainImporter.importLinearChain(plugins, "ns-test", "g-import", 1L);
        assertThat(graph.getNodes().size(), is(3));
        assertThat(graph.getNodes().get(1).getPluginName(), is("divide"));
    }

    @Test
    public void testImportEmptyChainYieldsSingleEnd() {
        final GraphData graph = LegacyChainImporter.importLinearChain(Collections.emptyList(), "ns-test", "g-empty", 1L);
        assertThat(graph.getEntryNodeId(), is(LegacyChainImporter.END_NODE_ID));
        assertThat(graph.getNodes().size(), is(1));
        assertThat(graph.getNodes().get(0).getType(), is(GraphNodeTypeEnum.END.getName()));
        assertThat(validator.validate(graph), is(empty()));
    }

    @Test
    public void testImportedGraphPassesValidationAndCompiles() {
        final List<PluginData> plugins = Arrays.asList(
                plugin("global", -1, true),
                plugin("sign", 20, true),
                plugin("divide", 200, true),
                plugin("response", 420, true));
        final GraphData graph = LegacyChainImporter.importLinearChain(plugins, "ns-test", "g-import", 1L);
        assertThat(validator.validate(graph), is(empty()));
        final GraphPlan plan = new GraphCompiler(validator).compile(graph);
        assertThat(plan.getEntryNodeId(), is("n0"));
        assertThat(plan.getSuccessors("n3"), is(Collections.singletonList(LegacyChainImporter.END_NODE_ID)));
    }

    @Test
    public void testContentHashIsDeterministicAndContentSensitive() {
        final List<PluginData> plugins = Arrays.asList(plugin("global", -1, true), plugin("divide", 200, true));
        final GraphData first = LegacyChainImporter.importLinearChain(plugins, "ns-test", "g-import", 1L);
        final GraphData second = LegacyChainImporter.importLinearChain(plugins, "ns-test", "g-import", 1L);
        assertThat(first.getContentHash(), is(second.getContentHash()));

        final GraphData changed = LegacyChainImporter.importLinearChain(
                Arrays.asList(plugin("global", -1, true), plugin("response", 420, true)), "ns-test", "g-import", 1L);
        assertThat(changed.getContentHash(), is(not(first.getContentHash())));
    }

    private PluginData plugin(final String name, final int sort, final boolean enabled) {
        return PluginData.builder().id(name).name(name).role("0").enabled(enabled).sort(sort).build();
    }
}
