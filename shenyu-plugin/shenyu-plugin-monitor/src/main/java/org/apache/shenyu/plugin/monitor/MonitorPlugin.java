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

package org.apache.shenyu.plugin.monitor;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.metrics.prometheus.register.PrometheusMetricsRegister;
import org.apache.shenyu.metrics.reporter.MetricsReporter;
import org.apache.shenyu.plugin.api.SoulPluginChain;
import org.apache.shenyu.plugin.api.context.SoulContext;
import org.apache.shenyu.plugin.base.AbstractSoulPlugin;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
import java.util.Optional;

/**
 * the monitor plugin.
 *
 * @author xiaoyu(Myth)
 */
public class MonitorPlugin extends AbstractSoulPlugin {
    
    private static final String REQUEST_TOTAL = "soul_request_total";
    
    private static final String HTTP_REQUEST_TOTAL = "soul_http_request_total";
    
    private static final String EXECUTE_LATENCY_NAME = "soul_execute_latency_millis";
    
    static {
        MetricsReporter.register(new PrometheusMetricsRegister());
        MetricsReporter.registerCounter(REQUEST_TOTAL, "soul request total count");
        MetricsReporter.registerCounter(HTTP_REQUEST_TOTAL, new String[]{"path", "type"}, "soul http request type total count");
        MetricsReporter.registerHistogram(EXECUTE_LATENCY_NAME, "the soul executor latency millis");
    }
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        MetricsReporter.counterIncrement(REQUEST_TOTAL);
        MetricsReporter.counterIncrement(HTTP_REQUEST_TOTAL, new String[]{exchange.getRequest().getURI().getPath(), exchange.getRequest().getMethodValue()});
        SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        LocalDateTime startDateTime = Optional.ofNullable(soulContext).map(SoulContext::getStartDateTime).orElse(LocalDateTime.now());
        return chain.execute(exchange).doOnSuccess(e -> responseCommitted(exchange, startDateTime))
                .doOnError(throwable -> responseCommitted(exchange, startDateTime));
    }
    
    @Override
    public int getOrder() {
        return PluginEnum.MONITOR.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.MONITOR.getName();
    }
    
    private void responseCommitted(final ServerWebExchange exchange, final LocalDateTime startDateTime) {
        ServerHttpResponse response = exchange.getResponse();
        if (response.isCommitted()) {
            recordTime(startDateTime);
        } else {
            response.beforeCommit(() -> {
                recordTime(startDateTime);
                return Mono.empty();
            });
        }
    }
    
    private void recordTime(final LocalDateTime startDateTime) {
        long millisBetween = DateUtils.acquireMillisBetween(startDateTime, LocalDateTime.now());
        MetricsReporter.recordTime(EXECUTE_LATENCY_NAME, millisBetween);
    }
}
