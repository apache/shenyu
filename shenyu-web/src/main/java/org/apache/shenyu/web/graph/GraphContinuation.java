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

import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Successor adapter with a provisional one-shot policy for issue #8015 review.
 * Repeated use fails explicitly; neither results nor side effects are replayed.
 * Transport-internal retry is unaffected. This is not a general retry contract.
 */
final class GraphContinuation implements ShenyuPluginChain {

    private final GraphExecution execution;

    private final String nextNodeId;

    private final AtomicBoolean consumed = new AtomicBoolean();

    GraphContinuation(final GraphExecution execution, final String nextNodeId) {
        this.execution = execution;
        this.nextNodeId = nextNodeId;
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange) {
        return Mono.defer(() -> {
            if (!consumed.compareAndSet(false, true)) {
                return Mono.error(new IllegalStateException("Graph continuation already consumed: " + nextNodeId));
            }
            return execution.run(nextNodeId, exchange);
        });
    }

    @Override
    public ShenyuPluginChain fork() {
        return execution.fork();
    }
}
