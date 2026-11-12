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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import reactor.core.publisher.Mono;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

/**
 * Request-scoped mutable execution state shared by primary, retry and fallback attempts.
 *
 * @param requestBody original client request body
 * @param responseConsumer client response boundary
 * @param emitted whether response bytes have reached the client
 * @param attemptIndex attempt sequence shared by primary and fallback
 * @param invocationId opaque logical invocation identifier
 * @param deadlineNanos absolute shared request deadline
 * @param observation subscription-local terminal observation
 */
record AiExecutionState(
        String requestBody,
        Function<AiUpstreamResponse, Mono<Void>> responseConsumer,
        AtomicBoolean emitted,
        AtomicInteger attemptIndex,
        String invocationId,
        long deadlineNanos,
        AiRequestObservation observation) {
}
