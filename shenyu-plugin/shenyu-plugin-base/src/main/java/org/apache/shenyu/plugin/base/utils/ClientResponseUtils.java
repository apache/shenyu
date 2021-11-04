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

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.reactive.function.client.ClientResponse;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * ClientResponseUtils.
 */
public final class ClientResponseUtils {

    /**
     * build client response with current response data.
     *
     * @param response current response
     * @param body current response body
     * @return the client respone
     */
    public static ClientResponse buildClientResponse(final ServerHttpResponse response,
                                                     final Publisher<? extends DataBuffer> body) {
        ClientResponse.Builder builder = ClientResponse.create(Objects.requireNonNull(response.getStatusCode()),
                ServerCodecConfigurer.create().getReaders());
        return builder.headers(headers -> headers.putAll(response.getHeaders())).body(Flux.from(body)).build();
    }

    /**
     * fix the body message.
     *
     * @param response current response
     * @param outputMessage cache message
     * @return fixed body message
     */
    public static Mono<DataBuffer> fixBodyMessage(final ServerHttpResponse response,
                                                  final CachedBodyOutputMessage outputMessage) {
        Mono<DataBuffer> messageBody = DataBufferUtils.join(outputMessage.getBody());
        HttpHeaders headers = response.getHeaders();
        if (!headers.containsKey(HttpHeaders.TRANSFER_ENCODING)
                || headers.containsKey(HttpHeaders.CONTENT_LENGTH)) {
            messageBody = messageBody.doOnNext(data -> headers.setContentLength(data.readableByteCount()));
        }
        return messageBody;
    }
}
