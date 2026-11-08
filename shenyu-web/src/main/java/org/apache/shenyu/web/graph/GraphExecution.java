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

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * State for one root or fork subscription. Never shared across entry subscriptions.
 */
final class GraphExecution {

    private final GraphExecutor executor;

    private final BoundGraphPlan plan;

    private final int depth;

    GraphExecution(final GraphExecutor executor, final BoundGraphPlan plan, final int depth) {
        this.executor = executor;
        this.plan = plan;
        this.depth = depth;
    }

    Mono<Void> run(final String id, final ServerWebExchange exchange) {
        return Mono.defer(() -> {
            BoundGraphPlan.Node node = plan.node(id);
            switch (node.type()) {
                case END:
                    // Response ownership stays with the surrounding plugin publisher.
                    return Mono.empty();
                case CHOICE:
                    return run(node.next(exchange), exchange);
                case PLUGIN:
                    ShenyuPlugin plugin = node.plugin();
                    String next = node.next(exchange);
                    if (plugin.skip(exchange)) {
                        return run(next, exchange);
                    }
                    try {
                        plugin.before(exchange);
                        return plugin.execute(exchange, new GraphContinuation(this, next));
                    } finally {
                        // Match the legacy construction-time hook, not asynchronous completion.
                        plugin.after(exchange);
                    }
                default:
                    return Mono.error(new IllegalStateException("Unsupported graph node " + id));
            }
        });
    }

    ShenyuPluginChain fork() {
        return executor.entry(plan, depth + 1);
    }
}
