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
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.function.Supplier;

/**
 * Selects a complete plugin execution path before any plugin runs.
 * The mode is fixed at construction; plan publication cannot change it.
 * Graph providers must admit complete pipelines and manage plugin lifetimes.
 */
public final class PluginExecutionDispatcher {

    private final Supplier<BoundGraphPlan> plans;

    private final String namespaceId;

    private final String graphId;

    private final String deploymentBinding;

    private final GraphExecutor executor;

    /**
     * Create the default LEGACY dispatcher.
     */
    public PluginExecutionDispatcher() {
        this.plans = null;
        this.namespaceId = null;
        this.graphId = null;
        this.deploymentBinding = null;
        this.executor = null;
    }

    /**
     * Create an explicit GRAPH dispatcher for an admitted plan provider.
     *
     * @param plans provider consulted once per entry subscription
     * @param namespaceId configured deployment namespace
     * @param graphId configured graph identity
     * @param deploymentBinding deployment identity, when required by the plan
     * @param executor independent graph executor
     */
    public PluginExecutionDispatcher(final Supplier<BoundGraphPlan> plans, final String namespaceId, final String graphId,
                                     final String deploymentBinding, final GraphExecutor executor) {
        this.plans = Objects.requireNonNull(plans, "plans");
        this.namespaceId = Objects.requireNonNull(namespaceId, "namespaceId");
        this.graphId = Objects.requireNonNull(graphId, "graphId");
        this.deploymentBinding = deploymentBinding;
        this.executor = Objects.requireNonNull(executor, "executor");
    }

    /**
     * Select and pin an execution before scheduling or plugin invocation.
     * Must be called inside the handler's entry defer, once per subscription.
     *
     * @param exchange request
     * @param legacy factory for the unchanged legacy cursor
     * @return selected pipeline, with no cross-executor fallback
     */
    public Mono<Void> execute(final ServerWebExchange exchange, final Supplier<ShenyuPluginChain> legacy) {
        try {
            if (Objects.isNull(plans)) {
                return legacy.get().execute(exchange);
            }
            BoundGraphPlan plan = plans.get();
            if (Objects.isNull(plan) || !namespaceId.equals(plan.getNamespaceId()) || !graphId.equals(plan.getGraphId())
                    || (Objects.nonNull(plan.getDeploymentBinding()) && !plan.getDeploymentBinding().isBlank()
                    && !plan.getDeploymentBinding().equals(deploymentBinding))) {
                return Mono.error(new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "Graph plan is not ready for this deployment"));
            }
            return executor.execute(plan, exchange);
        } catch (RuntimeException ex) {
            return Mono.error(ex);
        }
    }
}
