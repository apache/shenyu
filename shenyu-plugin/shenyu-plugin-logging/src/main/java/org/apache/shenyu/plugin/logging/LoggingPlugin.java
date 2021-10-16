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

package org.apache.shenyu.plugin.logging;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
import reactor.util.annotation.NonNull;

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

/**
 * Shenyu logging plugin. it can print request info(include request headers, request params, request body ...etc) and
 * response info(include response headers and response body).
 */
public class LoggingPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingPlugin.class);
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        ServerHttpRequest request = exchange.getRequest();
        StringBuilder requestInfo = new StringBuilder("Print Request Info: ").append(System.lineSeparator());
        requestInfo.append(getRequestUri(request)).append(getRequestMethod(request)).append(System.lineSeparator())
                .append(getRequestHeaders(request)).append(System.lineSeparator())
                .append(getQueryParams(request)).append(System.lineSeparator());
        return chain.execute(exchange.mutate().request(new LoggingServerHttpRequest(request, requestInfo))
                .response(new LoggingServerHttpResponse(exchange.getResponse(), requestInfo)).build());
    }
    
    @Override
    public int getOrder() {
        return PluginEnum.LOGGING.getCode();
    }
    
    @Override
    public String named() {
        return PluginEnum.LOGGING.getName();
    }
    
    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return false;
    }
    
    private String getRequestMethod(final ServerHttpRequest request) {
        return "Request Method: " + request.getMethod() + System.lineSeparator();
    }
    
    private String getRequestUri(final ServerHttpRequest request) {
        return "Request Uri: " + request.getURI() + System.lineSeparator();
    }
    
    private String getQueryParams(final ServerHttpRequest request) {
        MultiValueMap<String, String> params = request.getQueryParams();
        StringBuilder logInfo = new StringBuilder();
        if (!params.isEmpty()) {
            logInfo.append("[Query Params Start]").append(System.lineSeparator());
            params.forEach((key, value) -> logInfo.append(key).append(":").append(StringUtils.join(value, ",")).append(System.lineSeparator()));
            logInfo.append("[Query Params End]").append(System.lineSeparator());
        }
        return logInfo.toString();
    }
    
    private String getRequestHeaders(final ServerHttpRequest request) {
        HttpHeaders headers = request.getHeaders();
        final StringBuilder logInfo = new StringBuilder();
        if (!headers.isEmpty()) {
            logInfo.append("[Request Headers Start]").append(System.lineSeparator());
            logInfo.append(getHeaders(headers));
            logInfo.append("[Request Headers End]").append(System.lineSeparator());
        }
        return logInfo.toString();
    }
    
    private void print(final String info) {
        LOG.info(info);
    }
    
    private String getHeaders(final HttpHeaders headers) {
        StringBuilder sb = new StringBuilder();
        Set<Map.Entry<String, List<String>>> entrySet = headers.entrySet();
        for (Map.Entry<String, List<String>> entry : entrySet) {
            String key = entry.getKey();
            List<String> value = entry.getValue();
            sb.append(key).append(": ").append(StringUtils.join(value, ",")).append(System.lineSeparator());
        }
        return sb.toString();
    }
    
    static class LoggingServerHttpRequest extends ServerHttpRequestDecorator {

        private final StringBuilder logInfo;

        LoggingServerHttpRequest(final ServerHttpRequest delegate, final StringBuilder logInfo) {
            super(delegate);
            this.logInfo = logInfo;
        }

        @Override
        @NonNull
        public Flux<DataBuffer> getBody() {
            BodyWriter writer = new BodyWriter();
            return super.getBody().doOnNext(dataBuffer -> writer.write(dataBuffer.asByteBuffer().asReadOnlyBuffer())).doFinally(signal -> {
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

        private final StringBuilder logInfo;

        private final ServerHttpResponse serverHttpResponse;

        LoggingServerHttpResponse(final ServerHttpResponse delegate, final StringBuilder logInfo) {
            super(delegate);
            this.logInfo = logInfo;
            this.serverHttpResponse = delegate;
            this.logInfo.append(System.lineSeparator());
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            return super.writeWith(appendResponse(body));
        }

        @NonNull
        private Flux<? extends DataBuffer> appendResponse(final Publisher<? extends DataBuffer> body) {
            logInfo.append(System.lineSeparator());
            logInfo.append("Response Code: ").append(this.serverHttpResponse.getStatusCode()).append(System.lineSeparator());
            logInfo.append(getResponseHeaders()).append(System.lineSeparator());
            BodyWriter writer = new BodyWriter();
            return Flux.from(body).doOnNext(buffer -> writer.write(buffer.asByteBuffer().asReadOnlyBuffer())).doFinally(signal -> {
                logInfo.append("[Response Body Start]").append(System.lineSeparator());
                logInfo.append(writer.output()).append(System.lineSeparator());
                logInfo.append("[Response Body End]").append(System.lineSeparator());
                // when response, print all request info.
                print(logInfo.toString());
            });
        }

        private String getResponseHeaders() {
            return System.lineSeparator() + "[Response Headers Start]" + System.lineSeparator()
                + LoggingPlugin.this.getHeaders(serverHttpResponse.getHeaders())
                + "[Response Headers End]" + System.lineSeparator();
        }
    }

    static class BodyWriter {

        private final ByteArrayOutputStream stream = new ByteArrayOutputStream();

        private final WritableByteChannel channel = Channels.newChannel(stream);

        private final AtomicBoolean isClosed = new AtomicBoolean(false);

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

        boolean isEmpty() {
            return stream.size() == 0;
        }

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
