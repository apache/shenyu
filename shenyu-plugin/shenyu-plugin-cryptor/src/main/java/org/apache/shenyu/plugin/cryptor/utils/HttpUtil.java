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

package org.apache.shenyu.plugin.cryptor.utils;

import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * http util.
 */
public class HttpUtil {

    private static final String CHUNKED = "chunked";

    /**
     * change header.
     * @param headers headers
     * @return HttpHeaders.
     */
    public static HttpHeaders modifiedContentLength(final HttpHeaders headers) {
        long contentLength = headers.getContentLength();
        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.putAll(headers);
        if (contentLength > 0) {
            httpHeaders.setContentLength(contentLength);
        } else {
            httpHeaders.set(HttpHeaders.TRANSFER_ENCODING, CHUNKED);
        }
        return httpHeaders;
    }

    /**
     * create CachedBodyOutputMessage.
     * @param exchange ServerWebExchange
     * @return CachedBodyOutputMessage.
     */
    public static CachedBodyOutputMessage newCachedBodyOutputMessage(final ServerWebExchange exchange) {
        HttpHeaders headers = new HttpHeaders();
        headers.putAll(exchange.getRequest().getHeaders());
        headers.remove(HttpHeaders.CONTENT_LENGTH);
        return new CachedBodyOutputMessage(exchange, headers);
    }

    /**
     * release source.
     * @param outputMessage CachedBodyOutputMessage
     * @param throwable Throwable
     * @return Mono.
     */
    public static Mono<Void> release(final CachedBodyOutputMessage outputMessage, final Throwable throwable) {
        if (outputMessage.getCache()) {
            return outputMessage.getBody().map(DataBufferUtils::release).then(Mono.error(throwable));
        }
        return Mono.error(throwable);
    }
}
