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

/**
 * The resource limits applied when validating and compiling a plugin orchestration graph.
 *
 * <p>Defaults are conservative and configurable; they bound compile cost, plan memory,
 * retained-revision memory and internal-dispatch recursion. The values mirror the limit table in
 * {@code docs/plugin-graph-orchestration.md} (issue #7994) section 8.</p>
 *
 * @since 2.7.2
 */
public final class GraphLimits {

    /**
     * default maximum number of nodes per graph.
     */
    public static final int DEFAULT_MAX_NODES = 128;

    /**
     * default maximum out-degree (CHOICE branches).
     */
    public static final int DEFAULT_MAX_OUT_DEGREE = 16;

    /**
     * default maximum conditions per branch.
     */
    public static final int DEFAULT_MAX_CONDITIONS_PER_BRANCH = 32;

    /**
     * default maximum path length (prefix + branch + suffix).
     */
    public static final int DEFAULT_MAX_PATH_LENGTH = 64;

    /**
     * default maximum fork / internal-dispatch recursion depth.
     */
    public static final int DEFAULT_MAX_FORK_RECURSION_DEPTH = 8;

    /**
     * default maximum retained revisions per graph.
     */
    public static final int DEFAULT_MAX_RETAINED_REVISIONS = 8;

    /**
     * default maximum graphs per namespace.
     */
    public static final int DEFAULT_MAX_GRAPHS_PER_NAMESPACE = 32;

    private static final GraphLimits DEFAULT = new GraphLimits(DEFAULT_MAX_NODES, DEFAULT_MAX_OUT_DEGREE,
            DEFAULT_MAX_CONDITIONS_PER_BRANCH, DEFAULT_MAX_PATH_LENGTH, DEFAULT_MAX_FORK_RECURSION_DEPTH,
            DEFAULT_MAX_RETAINED_REVISIONS, DEFAULT_MAX_GRAPHS_PER_NAMESPACE);

    private final int maxNodes;

    private final int maxOutDegree;

    private final int maxConditionsPerBranch;

    private final int maxPathLength;

    private final int maxForkRecursionDepth;

    private final int maxRetainedRevisions;

    private final int maxGraphsPerNamespace;

    /**
     * all args constructor.
     *
     * @param maxNodes               maximum number of nodes per graph
     * @param maxOutDegree           maximum CHOICE branch fan-out
     * @param maxConditionsPerBranch maximum conditions per branch
     * @param maxPathLength          maximum root-to-END path length
     * @param maxForkRecursionDepth  maximum fork / internal-dispatch recursion depth
     * @param maxRetainedRevisions   maximum retained revisions per graph
     * @param maxGraphsPerNamespace  maximum graphs per namespace
     */
    public GraphLimits(final int maxNodes, final int maxOutDegree, final int maxConditionsPerBranch,
                       final int maxPathLength, final int maxForkRecursionDepth, final int maxRetainedRevisions,
                       final int maxGraphsPerNamespace) {
        this.maxNodes = maxNodes;
        this.maxOutDegree = maxOutDegree;
        this.maxConditionsPerBranch = maxConditionsPerBranch;
        this.maxPathLength = maxPathLength;
        this.maxForkRecursionDepth = maxForkRecursionDepth;
        this.maxRetainedRevisions = maxRetainedRevisions;
        this.maxGraphsPerNamespace = maxGraphsPerNamespace;
    }

    /**
     * The shared default limits.
     *
     * @return the default {@link GraphLimits}
     */
    public static GraphLimits defaults() {
        return DEFAULT;
    }

    /**
     * get maxNodes.
     *
     * @return maxNodes
     */
    public int getMaxNodes() {
        return maxNodes;
    }

    /**
     * get maxOutDegree.
     *
     * @return maxOutDegree
     */
    public int getMaxOutDegree() {
        return maxOutDegree;
    }

    /**
     * get maxConditionsPerBranch.
     *
     * @return maxConditionsPerBranch
     */
    public int getMaxConditionsPerBranch() {
        return maxConditionsPerBranch;
    }

    /**
     * get maxPathLength.
     *
     * @return maxPathLength
     */
    public int getMaxPathLength() {
        return maxPathLength;
    }

    /**
     * get maxForkRecursionDepth.
     *
     * @return maxForkRecursionDepth
     */
    public int getMaxForkRecursionDepth() {
        return maxForkRecursionDepth;
    }

    /**
     * get maxRetainedRevisions.
     *
     * @return maxRetainedRevisions
     */
    public int getMaxRetainedRevisions() {
        return maxRetainedRevisions;
    }

    /**
     * get maxGraphsPerNamespace.
     *
     * @return maxGraphsPerNamespace
     */
    public int getMaxGraphsPerNamespace() {
        return maxGraphsPerNamespace;
    }
}
