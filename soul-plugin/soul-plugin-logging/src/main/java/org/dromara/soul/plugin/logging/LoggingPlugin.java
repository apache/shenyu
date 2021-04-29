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

package org.dromara.soul.plugin.logging;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.WritableByteChannel;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.base.AbstractSoulPlugin;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * Soul logging plugin. it can print request info(include request headers, request params, request body ...etc) and
 * response info(include response headers and response body).
 *
 * @author xuxd
 **/
@Slf4j
public class LoggingPlugin extends AbstractSoulPlugin {

    @Override protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain,
        final SelectorData selector, final RuleData rule) {

        ServerHttpRequest request = exchange.getRequest();
        StringBuilder requestInfo = new StringBuilder("Print Request Info: ").append(System.lineSeparator());

        getRequestUri(request, requestInfo);
        getRequestMethod(request, requestInfo);
        requestInfo.append(System.lineSeparator());

        getRequestHeaders(request, requestInfo);
        requestInfo.append(System.lineSeparator());

        getQueryParams(request, requestInfo);
        requestInfo.append(System.lineSeparator());

        return chain
            .execute(
                exchange
                    .mutate()
                    .request(new LoggingServerHttpRequest(request, requestInfo))
                    .response(new LoggingServerHttpResponse(exchange.getResponse(), requestInfo))
                    .build()
            );
    }

    private void getRequestMethod(final ServerHttpRequest request, final StringBuilder debugInfo) {
        debugInfo
            .append("Request Method: ")
            .append(request.getMethod())
            .append(System.lineSeparator());
    }

    private void getRequestUri(final ServerHttpRequest request, final StringBuilder debugInfo) {
        debugInfo
            .append("Request Uri: ")
            .append(request.getURI())
            .append(System.lineSeparator());
    }

    private void getQueryParams(final ServerHttpRequest request, final StringBuilder debugInfo) {
        MultiValueMap<String, String> params = request.getQueryParams();
        if (params == null || params.isEmpty()) {
            return;
        }
        debugInfo.append("[Query Params Start]").append(System.lineSeparator());
        params
            .entrySet()
            .stream()
            .forEach(entry -> {
                debugInfo
                    .append(entry.getKey()).append(": ")
                    .append(StringUtils.join(entry.getValue(), ","))
                    .append(System.lineSeparator());
            });
        debugInfo.append("[Query Params End]").append(System.lineSeparator());
    }

    private void getRequestHeaders(final ServerHttpRequest request, final StringBuilder debugInfo) {
        HttpHeaders headers = request.getHeaders();
        if (!headers.isEmpty()) {
            debugInfo.append("[Request Headers Start]").append(System.lineSeparator());
            debugInfo.append(getHeader(headers));
            debugInfo.append("[Request Headers End]").append(System.lineSeparator());
        }
    }

    private void print(final String info) {
        log.info(info);
    }

    private String getHeader(final HttpHeaders headers) {
        StringBuilder sb = new StringBuilder();
        Set<Map.Entry<String, List<String>>> entrySet = headers.entrySet();
        for (Map.Entry<String, List<String>> entry : entrySet) {
            String key = entry.getKey();
            List<String> value = entry.getValue();
            sb
                .append(key)
                .append(": ")
                .append(StringUtils.join(value, ","))
                .append(System.lineSeparator());
        }
        return sb.toString();
    }

    @Override public int getOrder() {
        return PluginEnum.Logging.getCode();
    }

    @Override public String named() {
        return PluginEnum.Logging.getName();
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        return false;
    }

    class LoggingServerHttpRequest extends ServerHttpRequestDecorator {

        private StringBuilder logInfo;

        LoggingServerHttpRequest(final ServerHttpRequest delegate, final StringBuilder logInfo) {
            super(delegate);
            this.logInfo = logInfo;
        }

        @Override public Flux<DataBuffer> getBody() {
            BodyWriter writer = new BodyWriter();
            return super.getBody().doOnNext(dataBuffer -> {
                ByteBuffer buffer = dataBuffer.asByteBuffer().asReadOnlyBuffer();
                writer.write(buffer);
            }).doFinally(signal -> {
                if (!writer.isEmpty()) {
                    logInfo.append("[Request Body Start]").append(System.lineSeparator());
                    logInfo.append(writer.output()).append(System.lineSeparator());
                    logInfo.append("[Request Body End]").append(System.lineSeparator());
                } else {
                    // close writer when output.
                    writer.output();
                }
            });
        }
    }

    class LoggingServerHttpResponse extends ServerHttpResponseDecorator {

        private StringBuilder logInfo;

        private ServerHttpResponse serverHttpResponse;

        LoggingServerHttpResponse(final ServerHttpResponse delegate, final StringBuilder logInfo) {
            super(delegate);
            this.logInfo = logInfo;
            this.serverHttpResponse = delegate;
            this.logInfo.append(System.lineSeparator());
        }

        @Override public Mono<Void> writeWith(final Publisher<? extends DataBuffer> body) {
            return super.writeWith(appendResponse(body));
        }

        private Flux<? extends DataBuffer> appendResponse(final Publisher<? extends DataBuffer> body) {
            logInfo.append(System.lineSeparator());
            logInfo.append("Response Code: ").append(this.serverHttpResponse.getStatusCode()).append(System.lineSeparator());
            logInfo.append(getResponseHeaders()).append(System.lineSeparator());
            BodyWriter writer = new BodyWriter();
            Flux<? extends DataBuffer> flux = Flux.from(body).doOnNext(buffer -> {
                ByteBuffer byteBuffer = buffer.asByteBuffer().asReadOnlyBuffer();
                writer.write(byteBuffer);
            }).doFinally(signal -> {
                logInfo.append("[Response Body Start]").append(System.lineSeparator());
                logInfo.append(writer.output()).append(System.lineSeparator());
                logInfo.append("[Response Body End]").append(System.lineSeparator());
                print(logInfo.toString());
            });
            return flux;
        }

        private String getResponseHeaders() {
            StringBuilder sb = new StringBuilder(System.lineSeparator());
            sb.append("[Response Headers Start]").append(System.lineSeparator());
            sb.append(getHeader(serverHttpResponse.getHeaders()));
            sb.append("[Response Headers End]").append(System.lineSeparator());
            return sb.toString();
        }
    }

    class BodyWriter {

        private ByteArrayOutputStream baos = new ByteArrayOutputStream();

        private WritableByteChannel channel = Channels.newChannel(baos);

        private AtomicBoolean isClosed = new AtomicBoolean(false);

        void write(final ByteBuffer buffer) {
            if (!isClosed.get()) {
                try {
                    channel.write(buffer);
                } catch (IOException e) {
                    isClosed.compareAndSet(false, true);
                    log.error("Parse Failed.", e);
                }
            }
        }

        boolean isEmpty() {
            return baos.size() == 0;
        }

        String output() {
            try {
                isClosed.compareAndSet(false, true);
                return new String(baos.toByteArray(), StandardCharsets.UTF_8);
            } catch (Exception e) {
                log.error("Write failed: ", e);
                return "Write failed: " + e.getMessage();
            } finally {

                try {
                    baos.close();
                } catch (IOException e) {
                    log.error("Close baos error: ", e);
                }

                try {
                    channel.close();
                } catch (IOException e) {
                    log.error("Close channel error: ", e);
                }
            }

        }
    }
}
