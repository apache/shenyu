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

package org.apache.shenyu.agent.plugin.metrics.prometheus.handler;

import org.apache.shenyu.agent.api.entity.MethodResult;
import org.apache.shenyu.agent.api.entity.TargetObject;
import org.apache.shenyu.agent.api.handler.InstanceMethodHandler;
import org.apache.shenyu.agent.plugin.metrics.api.MetricsRecorder;
import org.apache.shenyu.agent.plugin.metrics.api.constant.MetricsConstant;
import org.apache.shenyu.agent.plugin.metrics.common.factory.MetricsRecorderPool;
import org.apache.shenyu.common.utils.DateUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.lang.reflect.Method;
import java.time.LocalDateTime;

/**
 * The type metrics prometheus global plugin handler.
 */
public final class PrometheusGlobalPluginHandler implements InstanceMethodHandler {

    @Override
    public void before(final TargetObject target, final Method method, final Object[] args, final MethodResult result) {
        MetricsRecorderPool.get(MetricsConstant.REQUEST_TOTAL, MetricsConstant.PROMETHEUS).ifPresent(MetricsRecorder::inc);
        MetricsRecorderPool.get(MetricsConstant.SHENYU_REQUEST_UNDONE, MetricsConstant.PROMETHEUS).ifPresent(MetricsRecorder::inc);

        final ServerWebExchange exchange = (ServerWebExchange) args[0];
        exchange.getAttributes().put(MetricsConstant.SHENYU_EXECUTE_LATENCY_MILLIS, LocalDateTime.now());
    }

    @Override
    public Object after(final TargetObject target, final Method method, final Object[] args, final MethodResult methodResult) {
        MetricsRecorderPool.get(MetricsConstant.SHENYU_REQUEST_UNDONE, MetricsConstant.PROMETHEUS).ifPresent(MetricsRecorder::dec);

        final ServerWebExchange exchange = (ServerWebExchange) args[0];
        final String path = exchange.getRequest().getURI().getPath();
        LocalDateTime startTime = (LocalDateTime) exchange.getAttributes().get(MetricsConstant.SHENYU_EXECUTE_LATENCY_MILLIS);
        Object result = methodResult.getResult();
        if (result instanceof Mono) {
            return ((Mono) result).doFinally(s -> {
                MetricsRecorderPool.get(MetricsConstant.SHENYU_EXECUTE_LATENCY_MILLIS, MetricsConstant.PROMETHEUS)
                        .ifPresent(metricsRecorder -> metricsRecorder.observe(DateUtils.acquireMillisBetween(startTime, LocalDateTime.now()), path));
            });
        }

        return result;
    }

    @Override
    public void onThrowing(final TargetObject target, final Method method, final Object[] args, final Throwable throwable) {
        MetricsRecorderPool.get(MetricsConstant.REQUEST_THROW_TOTAL, MetricsConstant.PROMETHEUS).ifPresent(MetricsRecorder::inc);
        MetricsRecorderPool.get(MetricsConstant.SHENYU_REQUEST_UNDONE, MetricsConstant.PROMETHEUS).ifPresent(MetricsRecorder::dec);
    }

}
