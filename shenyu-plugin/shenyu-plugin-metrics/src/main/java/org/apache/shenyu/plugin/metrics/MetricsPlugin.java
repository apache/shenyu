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

package org.apache.shenyu.plugin.metrics;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.CanaryContext;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.metrics.constant.CanaryMetric;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;
import org.apache.shenyu.plugin.metrics.reporter.MetricsReporter;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * the monitor plugin.
 */
public class MetricsPlugin implements ShenyuPlugin {

    private static final String CANARY_ROUTING_RECORDED = "canaryRoutingRecorded";

    private static final String CANARY_REQUEST_RECORDED = "canaryRequestRecorded";

    private static final String EXECUTE_LATENCY_RECORDED = "executeLatencyRecorded";

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        MetricsReporter.counterIncrement(LabelNames.REQUEST_TOTAL);
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        Objects.requireNonNull(shenyuContext);
        setMetricsCallbacks(exchange);
        MetricsReporter.counterIncrement(LabelNames.REQUEST_TYPE_TOTAL, new String[]{exchange.getRequest().getURI().getRawPath(), shenyuContext.getRpcType()});
        LocalDateTime startDateTime = Optional.of(shenyuContext).map(ShenyuContext::getStartDateTime).orElseGet(LocalDateTime::now);
        return chain.execute(exchange).doOnSuccess(e -> responseCommitted(exchange, startDateTime))
                .doOnError(throwable -> {
                    MetricsReporter.counterIncrement(LabelNames.REQUEST_THROW_TOTAL);
                    responseCommitted(exchange, startDateTime);
                })
                .doFinally(signal -> recordCanaryResult(exchange, signal));
    }

    private void setMetricsCallbacks(final ServerWebExchange exchange) {
        exchange.getAttributes().put(Constants.METRICS_SENTINEL, (Consumer<HttpStatus>) status -> {
            if (Objects.equals(HttpStatus.TOO_MANY_REQUESTS, status)) {
                MetricsReporter.counterIncrement(LabelNames.SENTINEL_REQUEST_RESTRICT_TOTAL);
            } else if (Objects.equals(HttpStatus.INTERNAL_SERVER_ERROR, status)) {
                MetricsReporter.counterIncrement(LabelNames.SENTINEL_REQUEST_CIRCUITBREAKER_TOTAL);
            }
        });
        exchange.getAttributes().put(Constants.METRICS_RESILIENCE4J, (Consumer<HttpStatus>) status -> {
            if (Objects.equals(HttpStatus.TOO_MANY_REQUESTS, status)) {
                MetricsReporter.counterIncrement(LabelNames.RESILIENCE4J_REQUEST_RESTRICT_TOTAL);
            } else if (Objects.equals(HttpStatus.INTERNAL_SERVER_ERROR, status)) {
                MetricsReporter.counterIncrement(LabelNames.RESILIENCE4J_REQUEST_CIRCUITBREAKER_TOTAL);
            }
        });
        exchange.getAttributes().put(Constants.METRICS_HYSTRIX, (Consumer<HttpStatus>) status -> {
            if (Objects.equals(HttpStatus.INTERNAL_SERVER_ERROR, status)) {
                MetricsReporter.counterIncrement(LabelNames.HYSTRIX_REQUEST_CIRCUITBREAKER_TOTAL);
            }
        });
        exchange.getAttributes().put(Constants.METRICS_RATE_LIMITER, (Consumer<HttpStatus>) status -> {
            if (Objects.nonNull(status) && HttpStatus.TOO_MANY_REQUESTS.equals(status)) {
                MetricsReporter.counterIncrement(LabelNames.RATELIMITER_REQUEST_RESTRICT_TOTAL);
            }
        });
        exchange.getAttributes().put(Constants.METRICS_CANARY, (Consumer<CanaryContext>) context -> recordCanaryRouting(exchange, context));
    }

    private void recordCanaryRouting(final ServerWebExchange exchange, final CanaryContext context) {
        if (Objects.isNull(exchange.getAttributes().putIfAbsent(CANARY_ROUTING_RECORDED, true))) {
            MetricsReporter.observe(CanaryMetric.DECISION_DURATION.getName(), CanaryMetric.DECISION_DURATION.getLabelValues(context, null),
                    context.getDecisionDurationNanos() / 1_000_000_000.0);
            if (Objects.nonNull(context.getFallbackReason())) {
                MetricsReporter.counterIncrement(CanaryMetric.FALLBACK.getName(), CanaryMetric.FALLBACK.getLabelValues(context, null));
            }
        }
    }

    private void recordCanaryResult(final ServerWebExchange exchange, final SignalType signal) {
        CanaryContext context = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        if (Objects.nonNull(context) && Objects.isNull(exchange.getAttributes().putIfAbsent(CANARY_REQUEST_RECORDED, true))) {
            recordCanaryRouting(exchange, context);
            MetricsReporter.counterIncrement(CanaryMetric.REQUESTS.getName(), CanaryMetric.REQUESTS.getLabelValues(context, canaryOutcome(exchange, context, signal)));
        }
    }

    private String canaryOutcome(final ServerWebExchange exchange, final CanaryContext context, final SignalType signal) {
        if (signal == SignalType.CANCEL) {
            return "cancelled";
        }
        if (Objects.nonNull(context.getRejectReason())) {
            return "reject";
        }
        if (signal == SignalType.ON_ERROR) {
            return "error";
        }
        HttpStatusCode status = exchange.getResponse().getStatusCode();
        return Objects.isNull(status) || status.is2xxSuccessful() || status.is3xxRedirection() ? "success" : "error";
    }

    @Override
    public int getOrder() {
        return PluginEnum.METRICS.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.METRICS.getName();
    }

    private void responseCommitted(final ServerWebExchange exchange, final LocalDateTime startDateTime) {
        ServerHttpResponse response = exchange.getResponse();
        if (response.isCommitted()) {
            recordTime(exchange, startDateTime);
        } else {
            response.beforeCommit(() -> Mono.fromRunnable(() -> recordTime(exchange, startDateTime)));
        }
    }

    private void recordTime(final ServerWebExchange exchange, final LocalDateTime startDateTime) {
        if (Objects.nonNull(exchange.getAttributes().putIfAbsent(EXECUTE_LATENCY_RECORDED, true))) {
            return;
        }
        long millisBetween = DateUtils.acquireMillisBetween(startDateTime, LocalDateTime.now());
        CanaryContext context = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        String partition = Objects.isNull(context) ? "none" : context.getMetricPartition();
        MetricsReporter.recordTime(LabelNames.EXECUTE_LATENCY_NAME, new String[]{partition}, millisBetween);
    }
}
