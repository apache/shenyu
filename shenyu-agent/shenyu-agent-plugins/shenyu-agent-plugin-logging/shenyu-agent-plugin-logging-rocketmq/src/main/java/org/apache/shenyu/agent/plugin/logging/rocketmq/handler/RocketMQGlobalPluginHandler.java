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

package org.apache.shenyu.agent.plugin.logging.rocketmq.handler;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.agent.api.entity.MethodResult;
import org.apache.shenyu.agent.api.entity.TargetObject;
import org.apache.shenyu.agent.api.handler.InstanceMethodHandler;
import org.apache.shenyu.agent.plugin.logging.entity.ShenyuRequestLog;
import org.apache.shenyu.agent.plugin.logging.rocketmq.RocketMQLogCollector;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.utils.HostAddressUtils;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.WritableByteChannel;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Method interceptor is used to  collect log.
 */
public class RocketMQGlobalPluginHandler implements InstanceMethodHandler {

    private static final Logger LOG = LoggerFactory.getLogger(RocketMQGlobalPluginHandler.class);

    private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS");

    private static final List<String> BINARY_TYPE_LIST = Arrays.asList("image", "multipart", "cbor",
            "octet-stream", "pdf");

    @Override
    public Object after(final TargetObject target, final Method method, final Object[] args,
                        final MethodResult methodResult, final Object result) {
        ServerWebExchange exchange = (ServerWebExchange) result;
        ServerHttpRequest request = exchange.getRequest();
        ServerHttpResponse response = exchange.getResponse();

        ShenyuRequestLog requestInfo = new ShenyuRequestLog();
        requestInfo.setRequestUri(request.getURI().toString());
        requestInfo.setMethod(request.getMethodValue());
        requestInfo.setRequestHeader(getHeaders(request.getHeaders()));
        requestInfo.setQueryParams(request.getURI().getQuery());
        requestInfo.setClientIp(HostAddressUtils.acquireIp(exchange));
        requestInfo.setUserAgent(request.getHeaders().getFirst("User-Agent"));
        requestInfo.setHost(request.getHeaders().getFirst("Host"));

        LoggingServerHttpRequest loggingServerHttpRequest = new LoggingServerHttpRequest(request, requestInfo);
        LoggingServerHttpResponse loggingServerHttpResponse = new LoggingServerHttpResponse(response, requestInfo);
        ServerWebExchange webExchange = exchange.mutate().request(loggingServerHttpRequest)
                .response(loggingServerHttpResponse).build();
        loggingServerHttpResponse.setExchange(webExchange);
        return webExchange;
    }
    

    /**
     * getHeader string.
     *
     * @return header
     */
    private static String getHeaders(final HttpHeaders headers) {
        StringBuilder sb = new StringBuilder();
        Set<Map.Entry<String, List<String>>> entrySet = headers.entrySet();
        entrySet.forEach(entry -> {
            String key = entry.getKey();
            List<String> value = entry.getValue();
            sb.append(key).append(": ").append(StringUtils.join(value, ",")).append(System.lineSeparator());
        });
        return sb.toString();
    }

    /**
     * judge this header is binary type.
     *
     * @param headers httpHeaders
     * @return true: is not binary type
     */
    private static boolean isNotBinaryType(final HttpHeaders headers) {
        return Optional.ofNullable(headers).map(HttpHeaders::getContentType)
                .map(contentType -> !BINARY_TYPE_LIST.contains(contentType.getType())
                        && !BINARY_TYPE_LIST.contains(contentType.getSubtype()))
                .orElse(true);
    }


    /**
     * wrap ServerHttpRequest.
     */
    static class LoggingServerHttpRequest extends ServerHttpRequestDecorator {
        private final ShenyuRequestLog logInfo;

        LoggingServerHttpRequest(final ServerHttpRequest delegate, final ShenyuRequestLog logInfo) {
            super(delegate);
            this.logInfo = logInfo;
        }

        /**
         * get request body.
         *
         * @return Flux
         */
        @Override
        @NonNull
        public Flux<DataBuffer> getBody() {
            BodyWriter writer = new BodyWriter();
            return super.getBody().doOnNext(dataBuffer -> writer.write(dataBuffer.asByteBuffer().asReadOnlyBuffer()))
                    .doFinally(signal -> {
                        if (!writer.isEmpty() && isNotBinaryType(getHeaders())) {
                            logInfo.setRequestBody(writer.output());
                        } else {
                            writer.output();
                        }
                    });
        }
    }

    /**
     * wrap serverHttpResponse.
     */
    static class LoggingServerHttpResponse extends ServerHttpResponseDecorator {

        private final ShenyuRequestLog logInfo;

        private ServerWebExchange exchange;

        LoggingServerHttpResponse(final ServerHttpResponse delegate, final ShenyuRequestLog logInfo) {
            super(delegate);
            this.logInfo = logInfo;
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
            logInfo.setResponseHeader(RocketMQGlobalPluginHandler.getHeaders(getHeaders()));
            BodyWriter writer = new BodyWriter();
            return Flux.from(body).doOnNext(buffer -> writer.write(buffer.asByteBuffer().asReadOnlyBuffer()))
                    .doFinally(signal -> logResponse(shenyuContext, writer));
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
            long costTime = DateUtils.acquireMillisBetween(shenyuContext.getStartDateTime(),
                    LocalDateTime.now());
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
            String body = writer.output();
            if (isNotBinaryType(getHeaders())) {
                logInfo.setResponseBody(body);
            }
            // collect log
            RocketMQLogCollector.getInstance().collect(logInfo);
        }
    }

    /**
     * bodyWriter is used to read Body.
     */
    static class BodyWriter {

        private final ByteArrayOutputStream stream = new ByteArrayOutputStream();

        private final WritableByteChannel channel = Channels.newChannel(stream);

        private final AtomicBoolean isClosed = new AtomicBoolean(false);


        /**
         * write ByteBuffer.
         *
         * @param buffer byte buffer
         */
        void write(final ByteBuffer buffer) {
            if (!isClosed.get()) {
                try {
                    channel.write(buffer);
                } catch (IOException e) {
                    isClosed.compareAndSet(false, true);
                    LOG.error("Parse Failed.", e);
                }
            }
        }

        /**
         * judge stream is empty.
         *
         * @return true: stream is empty
         */
        boolean isEmpty() {
            return stream.size() == 0;
        }

        /**
         * get stream size.
         *
         * @return size of stream
         */
        int size() {
            return stream.size();
        }

        /**
         * output stream value.
         *
         * @return string of stream
         */
        String output() {
            try {
                isClosed.compareAndSet(false, true);
                return new String(stream.toByteArray(), StandardCharsets.UTF_8);
            } catch (Exception e) {
                LOG.error("Write failed: ", e);
                return "Write failed: " + e.getMessage();
            } finally {
                try {
                    stream.close();
                } catch (IOException e) {
                    LOG.error("Close stream error: ", e);
                }
                try {
                    channel.close();
                } catch (IOException e) {
                    LOG.error("Close channel error: ", e);
                }
            }
        }
    }
}
