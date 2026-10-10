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

package org.apache.shenyu.plugin.ai.api.model;

import java.net.URI;
import java.nio.ByteBuffer;
import java.util.List;
import java.util.Map;

import reactor.core.publisher.Flux;

/**
 * Provider-prepared request data consumed by an AI transport.
 *
 * @param uri upstream endpoint
 * @param method HTTP method
 * @param headers upstream headers
 * @param body request body, which is consumed once by the transport
 */
public record AiUpstreamRequest(URI uri, String method, Map<String, List<String>> headers, Flux<ByteBuffer> body) {
}
