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

package org.apache.shenyu.plugin.ai.proxy.enhanced.transport;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.springframework.core.io.buffer.DataBuffer;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * Reactive transport for protocol-independent AI upstream requests.
 */
public interface AiProxyTransport {

    /**
     * Execute an upstream request.
     *
     * @param request upstream request
     * @return upstream response
     */
    Mono<AiUpstreamResponse> execute(AiUpstreamRequest request);

    /**
     * Prepare a one-shot upload. Validate local prerequisites before returning a cold publisher
     * that starts the upstream attempt on subscription.
     *
     * @param request destination, method and end-to-end headers
     * @param body original upload
     * @return a single-use publisher of the upstream response
     * @throws AiProxyException when streaming uploads are unsupported or request metadata is invalid
     * @throws AiTransportException when the declared size exceeds the upload budget
     */
    default Mono<AiUpstreamResponse> prepareStreaming(final AiUpstreamRequest request, final Flux<DataBuffer> body) {
        // 默认实现抛出不支持透传上传的异常。
        throw new AiProxyException(AiProxyException.Kind.PROVIDER, "AI transport does not support passthrough uploads", null);
    }
}
