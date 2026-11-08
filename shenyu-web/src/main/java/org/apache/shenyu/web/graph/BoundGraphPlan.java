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

import org.apache.shenyu.common.enums.GraphNodeTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

/**
 * Runtime snapshot containing copied topology and resolved plugin instances.
 * Plugin caches remain live; this snapshot does not own plugin resources.
 */
public final class BoundGraphPlan {

    private final String graphId;

    private final String namespaceId;

    private final String deploymentBinding;

    private final long revision;

    private final String entryNodeId;

    private final Map<String, Node> nodes;

    BoundGraphPlan(final String graphId, final String namespaceId, final String deploymentBinding,
                   final long revision, final String entryNodeId, final Map<String, Node> nodes) {
        this.graphId = graphId;
        this.namespaceId = namespaceId;
        this.deploymentBinding = deploymentBinding;
        this.revision = revision;
        this.entryNodeId = entryNodeId;
        this.nodes = Map.copyOf(nodes);
    }

    /**
     * Get graph identity.
     * @return graph id
     */
    public String getGraphId() {
        return graphId;
    }

    /**
     * Get namespace binding.
     * @return namespace id
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * Get optional deployment binding.
     * @return deployment binding
     */
    public String getDeploymentBinding() {
        return deploymentBinding;
    }

    /**
     * Get pinned revision.
     * @return revision
     */
    public long getRevision() {
        return revision;
    }

    String entryNodeId() {
        return entryNodeId;
    }

    Node node(final String id) {
        return nodes.get(id);
    }

    static final class Node {

        private final GraphNodeTypeEnum type;

        private final ShenyuPlugin plugin;

        private final String next;

        private final List<Branch> branches;

        Node(final GraphNodeTypeEnum type, final ShenyuPlugin plugin, final String next, final List<Branch> branches) {
            this.type = type;
            this.plugin = plugin;
            this.next = next;
            this.branches = List.copyOf(branches);
        }

        GraphNodeTypeEnum type() {
            return type;
        }

        ShenyuPlugin plugin() {
            return plugin;
        }

        String next(final ServerWebExchange exchange) {
            for (Branch branch : branches) {
                if (branch.matches.test(exchange)) {
                    return branch.target;
                }
            }
            return next;
        }
    }

    static final class Branch {

        private final Predicate<ServerWebExchange> matches;

        private final String target;

        Branch(final Predicate<ServerWebExchange> matches, final String target) {
            this.matches = matches;
            this.target = target;
        }
    }
}
