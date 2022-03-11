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

package org.apache.shenyu.agent.plugin.logging.common.body;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.agent.plugin.logging.LogCollector;
import org.apache.shenyu.agent.plugin.logging.common.utils.LogCollectConfigUtils;
import org.apache.shenyu.agent.plugin.logging.common.utils.LogCollectUtils;
import org.apache.shenyu.agent.plugin.logging.entity.ShenyuRequestLog;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URL;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Optional;

/**
 * decorate ServerHttpResponse for read body.
 */
public class LoggingServerHttpResponse extends ServerHttpResponseDecorator {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingServerHttpResponse.class);

    private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS");

    private static final String SHENYU_AGENT_TRACE_ID = "shenyu-agent-trace-id";

    private final ShenyuRequestLog logInfo;

    private ServerWebExchange exchange;

    private final LogCollector logCollector;

    public LoggingServerHttpResponse(final ServerHttpResponse delegate, final ShenyuRequestLog logInfo,
                                     final LogCollector logCollector) {
        super(delegate);
        this.logInfo = logInfo;
        this.logCollector = logCollector;
    }

    /**
     * set relevant ServerWebExchange.
     *
     * @param exchange ServerWebExchange
     */
    public void setExchange(final ServerWebExchange exchange) {
        this.exchange = exchange;
    }

    /**
     * write with a publisher.
     *
     * @param body response body
     * @return Mono
     */
    @Override
    @NonNull
    public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
        return super.writeWith(appendResponse(body));
    }

    /**
     * append response.
     *
     * @param body publisher
     * @return wrap Flux
     */
    @NonNull
    private Flux<? extends DataBuffer> appendResponse(final Publisher<? extends DataBuffer> body) {
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        if (getStatusCode() != null) {
            logInfo.setStatus(getStatusCode().value());
        }
        logInfo.setResponseHeader(LogCollectUtils.getResponseHeaders(getHeaders()));
        BodyWriter writer = new BodyWriter();
        String traceId = (String) exchange.getAttributes().get(SHENYU_AGENT_TRACE_ID);
        logInfo.setTraceId(traceId);
        boolean collectResponseBody = LogCollectConfigUtils.getLogFieldSwitchConfig().isResponseBody();
        return Flux.from(body).doOnNext(buffer -> {
            if (LogCollectUtils.isNotBinaryType(getHeaders()) && collectResponseBody) {
                writer.write(buffer.asByteBuffer().asReadOnlyBuffer());
            }
        }).doFinally(signal -> logResponse(shenyuContext, writer));
    }

    /**
     * record response log.
     *
     * @param shenyuContext request context
     * @param writer        bodyWriter
     */
    private void logResponse(final ShenyuContext shenyuContext, final BodyWriter writer) {
        if (StringUtils.isNotBlank(getHeaders().getFirst(HttpHeaders.CONTENT_LENGTH))) {
            logInfo.setResponseContentLength(getHeaders().getFirst(HttpHeaders.CONTENT_LENGTH));
        } else {
            logInfo.setResponseContentLength(writer.size() + "");
        }
        logInfo.setTimeLocal(shenyuContext.getStartDateTime().format(DATE_TIME_FORMATTER));
        long costTime = DateUtils.acquireMillisBetween(shenyuContext.getStartDateTime(), LocalDateTime.now());
        logInfo.setUpstreamResponseTime(costTime);
        if (StringUtils.isNotBlank(shenyuContext.getRpcType())) {
            logInfo.setRpcType(shenyuContext.getRpcType());
            if (RpcTypeEnum.HTTP.getName().equals(shenyuContext.getRpcType())) {
                URI uri = exchange.getAttribute(Constants.HTTP_URI);
                if (uri != null) {
                    logInfo.setUpstreamIp(uri.getHost());
                } else {
                    String domain = (String) exchange.getAttributes().get(Constants.HTTP_DOMAIN);
                    try {
                        URL url = new URL(domain);
                        logInfo.setUpstreamIp(url.getHost());
                    } catch (Exception e) {
                        LOG.error("get upstream ip error");
                    }
                }
            } else {
                Optional.ofNullable(exchange.getRequest().getRemoteAddress())
                        .map(InetSocketAddress::getAddress)
                        .ifPresent(v -> logInfo.setUpstreamIp(v.getHostAddress()));
                logInfo.setMethod(shenyuContext.getMethod());
            }
        }
        int size = writer.size();
        String body = writer.output();
        if (size == 0 || LogCollectConfigUtils.isResponseBodyTooLarge(size)) {
            return;
        }
        logInfo.setResponseBody(body);
        // collect log
        if (logCollector != null) {
            logCollector.collect(logInfo);
        }
    }
}
