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

/**
 * AI Proxy orchestration and configuration resolution.
 *
 * <p>The terminal plugin now invokes AiProxyEngine for both response modes. The
 * engine discovers the configured or detected Protocol, validates its Provider,
 * decodes and adapts the request, executes Transport, adapts the Provider response,
 * and encodes the Protocol response. The response consumer belongs to the plugin
 * and writes status, end-to-end headers and byte chunks to WebFlux.
 *
 * <p>The execute entry point accepts a response consumer that must subscribe
 * to the body once and return its completion. Keeping consumption in the same
 * reactive scope allows preflight of the first encoded chunk without detaching
 * the upstream subscription or accumulating the complete response. Cancellation
 * propagates through the consumer to the active Transport.
 *
 * <p>Normal requests retry transient transport failures and HTTP 429/5xx three
 * times with exponential backoff. Streaming requests retry once before their
 * first emitted event. A configured fallback is attempted once after primary
 * failure, rebuilding the original request with fallback credentials, endpoint,
 * model and generation settings. Request/provider validation and downstream
 * write failures are not retried. After any body emission, failures propagate;
 * the plugin does not replace an already committed response.
 *
 * <p>Selector/global configuration merging and dynamic fallback precedence are
 * retained. Proxy keys are validated in the plugin and replaced with real keys;
 * they are never forwarded to upstreams. The 5MB request limit remains in the
 * plugin. OpenAI Chat keeps unknown fields and incremental SSE events, emits
 * one [DONE] on successful completion, and does not synthesize completion after
 * a failed or cancelled stream. Upstream HTTP errors retain status and raw body.
 *
 * <p>Extensions register AiProxyProtocol and AiProxyProvider beans with unique
 * names. Protocol implementations own response encoding; Provider implementations
 * own request and response adaptation. A custom AiProxyTransport bean replaces
 * the default shared WebClient. Extensions must retain reactive body lifetime and
 * keep retry/fallback policy in the engine. Existing OpenAI, DeepSeek, ALiYun,
 * OpenAPI and Moonshot identifiers remain supported by compatible profiles.
 * Other AI plugins retain their Spring AI factories and dependencies.
 */
package org.apache.shenyu.plugin.ai.proxy.enhanced.service;
