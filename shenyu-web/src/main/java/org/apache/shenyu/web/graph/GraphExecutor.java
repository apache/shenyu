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

import org.apache.shenyu.common.graph.GraphLimits;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * Serial graph runtime. Every entry subscription creates independent state.
 */
public final class GraphExecutor {

    private final int maxForkDepth;

    /**
     * Create an executor with the contract's default fork limit.
     */
    public GraphExecutor() {
        this(GraphLimits.DEFAULT_MAX_FORK_RECURSION_DEPTH);
    }

    /**
     * Create an executor with a bounded fork depth.
     * @param maxForkDepth maximum child depth, with the root at zero
     */
    public GraphExecutor(final int maxForkDepth) {
        if (maxForkDepth < 0) {
            throw new IllegalArgumentException("maxForkDepth must not be negative");
        }
        this.maxForkDepth = maxForkDepth;
    }

    /**
     * Execute a pinned plan, without consulting any dispatcher or registry.
     * @param plan admitted and bound plan
     * @param exchange current request
     * @return complete plugin pipeline
     */
    public Mono<Void> execute(final BoundGraphPlan plan, final ServerWebExchange exchange) {
        return entry(Objects.requireNonNull(plan, "plan"), 0).execute(exchange);
    }

    ShenyuPluginChain entry(final BoundGraphPlan plan, final int depth) {
        return new ShenyuPluginChain() {
            @Override
            public Mono<Void> execute(final ServerWebExchange exchange) {
                return Mono.defer(() -> {
                    if (depth > maxForkDepth) {
                        return Mono.error(new IllegalStateException("Graph fork depth exceeds " + maxForkDepth));
                    }
                    return new GraphExecution(GraphExecutor.this, plan, depth).run(plan.entryNodeId(), exchange);
                });
            }

            @Override
            public ShenyuPluginChain fork() {
                return entry(plan, depth + 1);
            }
        };
    }
}
