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

import org.apache.shenyu.plugin.record.config.HttpRecordCollectConfig;
import org.apache.shenyu.plugin.record.entity.ShenyuHttpRequestRecord;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import reactor.core.publisher.Flux;
import reactor.util.annotation.NonNull;

/**
 * RecordServerHttpRequest.
 *
 * <p>Decorator that captures request body for recording.
 * Body capture is limited by maxBodySize to prevent OOM.</p>
 */
public class RecordServerHttpRequest extends ServerHttpRequestDecorator {

    private static final String TRUNCATED_PREFIX = "[TRUNCATED] ";

    private final ShenyuHttpRequestRecord record;

    private final int maxBodySize;

    /**
     * Constructor.
     *
     * @param delegate the original request
     * @param record the record object to populate
     */
    public RecordServerHttpRequest(final ServerHttpRequest delegate, final ShenyuHttpRequestRecord record) {
        super(delegate);
        this.record = record;
        this.maxBodySize = HttpRecordCollectConfig.INSTANCE.getRecordConfig().getMaxBodySize();
    }

    @Override
    @NonNull
    public Flux<DataBuffer> getBody() {
        BodyWriter writer = new BodyWriter(maxBodySize);
        return super.getBody().doOnNext(dataBuffer -> {
            try (DataBuffer.ByteBufferIterator bufferIterator = dataBuffer.readableByteBuffers()) {
                bufferIterator.forEachRemaining(byteBuffer -> writer.write(byteBuffer.asReadOnlyBuffer()));
            }
        }).doFinally(signal -> {
            if (writer.isSizeExceeded()) {
                record.setRequestBody(TRUNCATED_PREFIX + "request body exceeded max size of " + maxBodySize + " bytes");
            } else {
                record.setRequestBody(writer.output());
            }
        });
    }
}
