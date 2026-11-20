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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.function.Supplier;

/**
 * Shared bounded materialization of a full upstream body (issue #8257 review:
 * one limit, one accumulation, one overflow contract instead of duplicated
 * per-caller copies).
 */
public final class AiProxyBodies {

    /**
     * Maximum number of bytes an AI response body may occupy when a stage needs
     * the full body in memory; the validation and the protocol conversion share it.
     */
    public static final int MAX_BODY_BYTES = 5 * 1024 * 1024;

    private AiProxyBodies() {
    }

    /**
     * Materialize a full body into a UTF-8 string, bounded by {@link #MAX_BODY_BYTES}.
     *
     * @param body the response body flux, consumed exactly once
     * @param overflowError the classified failure to raise when the limit is exceeded
     * @return the materialized body
     */
    public static Mono<String> materialize(final Flux<byte[]> body, final Supplier<AiProxyException> overflowError) {
        return body.reduceWith(ByteArrayOutputStream::new, (buffer, bytes) -> {
            if (bytes.length > MAX_BODY_BYTES - buffer.size()) {
                throw overflowError.get();
            }
            buffer.writeBytes(bytes);
            return buffer;
        }).map(buffer -> buffer.toString(StandardCharsets.UTF_8));
    }
}
