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

package org.apache.shenyu.plugin.logging.common.body;

import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectUtils;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import reactor.core.publisher.Flux;
import reactor.util.annotation.NonNull;

/**
 * decorate ServerHttpRequest for read body.
 */
public class LoggingServerHttpRequest<L extends ShenyuRequestLog> extends ServerHttpRequestDecorator {

    private final L logInfo;

    private final int maxRequestBody;

    public LoggingServerHttpRequest(final ServerHttpRequest delegate, final L logInfo) {
        this(delegate, logInfo, GenericGlobalConfig.DEFAULT_MAX_REQUEST_BODY);
    }

    /**
     * Create a request decorator using the body limit captured for this request.
     *
     * @param delegate delegate request
     * @param logInfo access log
     * @param maxRequestBody maximum logged request body size in bytes
     */
    public LoggingServerHttpRequest(final ServerHttpRequest delegate, final L logInfo, final int maxRequestBody) {
        super(delegate);
        this.logInfo = logInfo;
        this.maxRequestBody = maxRequestBody;
    }

    /**
     * get request body.
     *
     * @return Flux
     */
    @Override
    @NonNull
    public Flux<DataBuffer> getBody() {
        BodyWriter writer = new BodyWriter(maxRequestBody);
        return super.getBody().doOnNext(dataBuffer -> {
            if (LogCollectUtils.isNotBinaryType(getHeaders())) {
                try (DataBuffer.ByteBufferIterator bufferIterator = dataBuffer.readableByteBuffers()) {
                    bufferIterator.forEachRemaining(byteBuffer -> writer.write(byteBuffer.asReadOnlyBuffer()));
                }

            }
        }).doFinally(signal -> {
            // use the writer's overflow state: size() saturates at Integer.MAX_VALUE
            if (writer.isEmpty() || writer.isLimitExceeded()) {
                return;
            }
            String body = writer.output();
            logInfo.setRequestBody(body);
        });
    }
}
