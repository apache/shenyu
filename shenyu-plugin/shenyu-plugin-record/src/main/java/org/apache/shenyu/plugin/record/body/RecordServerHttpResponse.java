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

package org.apache.shenyu.plugin.record.body;

import org.apache.shenyu.plugin.base.utils.MediaTypeUtils;
import org.apache.shenyu.plugin.record.collector.HttpRecordCollector;
import org.apache.shenyu.plugin.record.config.HttpRecordCollectConfig;
import org.apache.shenyu.plugin.record.entity.ShenyuHttpRequestRecord;
import org.apache.shenyu.plugin.record.utils.RecordUtils;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.util.Objects;

/**
 * RecordServerHttpResponse.
 *
 * <p>Decorator that captures response status, headers, and body for recording.
 * When the response body is binary type, only metadata is captured.</p>
 */
public class RecordServerHttpResponse extends ServerHttpResponseDecorator {

    private static final Logger LOG = LoggerFactory.getLogger(RecordServerHttpResponse.class);

    private static final String TRUNCATED_PREFIX = "[TRUNCATED] ";

    private final ShenyuHttpRequestRecord record;

    private final ServerWebExchange exchange;

    private final HttpRecordCollector httpRecordCollector;

    private final int maxBodySize;

    /**
     * Constructor.
     *
     * @param delegate the original response
     * @param record the record object to populate
     * @param httpRecordCollector the collector to submit the record to
     * @param exchange the server web exchange
     */
    public RecordServerHttpResponse(final ServerHttpResponse delegate, final ShenyuHttpRequestRecord record,
                                    final HttpRecordCollector httpRecordCollector,
                                    final ServerWebExchange exchange) {
        super(delegate);
        this.record = record;
        this.httpRecordCollector = httpRecordCollector;
        this.exchange = exchange;
        this.maxBodySize = HttpRecordCollectConfig.INSTANCE.getRecordConfig().getMaxBodySize();
    }

    @Override
    public Mono<Void> writeWith(final Publisher<? extends DataBuffer> body) {
        return super.writeWith(appendResponse(body));
    }

    @NonNull
    private Flux<? extends DataBuffer> appendResponse(final Publisher<? extends DataBuffer> body) {
        if (Objects.nonNull(getStatusCode())) {
            record.setStatus(getStatusCode().value());
        }
        record.setResponseHeaders(RecordUtils.getHeaders(getHeaders()));
        final MediaType mediaType = exchange.getResponse().getHeaders().getContentType();
        if (MediaTypeUtils.isByteType(mediaType)) {
            httpRecordCollector.collect(record);
            return Flux.from(body);
        }
        BodyWriter writer = new BodyWriter(maxBodySize);
        return Flux.from(body).doOnNext(buffer -> {
            if (RecordUtils.isNotBinaryType(getHeaders())) {
                try (DataBuffer.ByteBufferIterator bufferIterator = buffer.readableByteBuffers()) {
                    bufferIterator.forEachRemaining(byteBuffer -> writer.write(byteBuffer.asReadOnlyBuffer()));
                }
            }
        }).doFinally(signal -> {
            if (writer.isSizeExceeded()) {
                record.setResponseBody(TRUNCATED_PREFIX + "response body exceeded max size of " + maxBodySize + " bytes");
            } else {
                record.setResponseBody(writer.output());
            }
            httpRecordCollector.collect(record);
        });
    }
}
