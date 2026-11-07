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

package org.apache.shenyu.common.plugin.capability;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.apache.shenyu.common.plugin.capability.PluginCapabilityDescriptor.Body.BOUNDED_BUFFER;
import static org.apache.shenyu.common.plugin.capability.PluginCapabilityDescriptor.Body.BUFFERED;
import static org.apache.shenyu.common.plugin.capability.PluginCapabilityDescriptor.Body.NONE;
import static org.apache.shenyu.common.plugin.capability.PluginCapabilityDescriptor.Body.PASS_THROUGH;
import static org.apache.shenyu.common.plugin.capability.PluginCapabilityDescriptor.Body.STREAM_RELAY;
import static org.apache.shenyu.common.plugin.capability.PluginCapabilityDescriptor.ResponseEffect.COMMIT;
import static org.apache.shenyu.common.plugin.capability.PluginCapabilityDescriptor.ResponseEffect.CONSUME_RESULT;
import static org.apache.shenyu.common.plugin.capability.PluginCapabilityDescriptor.ResponseEffect.DECORATE;
import static org.apache.shenyu.common.plugin.capability.PluginCapabilityDescriptor.ResponseEffect.PRODUCE_RESULT;
import static org.apache.shenyu.common.plugin.capability.PluginCapabilityDescriptor.ResponseEffect.SHORT_CIRCUIT;

/** Audited cooperating HTTP roles; no implementation class is loaded while constructing these facts. */
final class HttpPilotCapabilities {

    private HttpPilotCapabilities() {
    }

    static List<PluginCapabilityDescriptor> declarations() {
        return List.of(
                descriptor("divide", "divide.DividePlugin", List.of(
                        mode("Matched usable HTTP route; request size checks count/relay without whole-body buffering", PASS_THROUGH, NONE),
                        mode("Selection or request validation rejects before forwarding", NONE, BUFFERED, SHORT_CIRCUIT, COMMIT)),
                        Set.of("http.domain"), Set.of(), Set.of("load-balancer"), "divide:RULE:1"),
                descriptor("uri", "uri.URIPlugin", List.of(
                        mode("HTTP_DOMAIN is present; constructs HTTP_URI, otherwise delegates", NONE, NONE)),
                        Set.of("http.uri"), Set.of("http.domain"), Set.of(), null),
                descriptor("nettyHttpClient", "httpclient.NettyHttpClientPlugin", List.of(
                        mode("HTTP_URI is available; transport response is relayed later by a configured response writer", STREAM_RELAY, STREAM_RELAY, PRODUCE_RESULT),
                        mode("Retries of a request with a body use the configured bounded replay cache", BOUNDED_BUFFER, STREAM_RELAY, PRODUCE_RESULT)),
                        Set.of("http.result"), Set.of("http.uri", "http.writer"), Set.of(), null),
                descriptor("webClient", "httpclient.WebClientPlugin", List.of(
                        mode("Selected WebClient with HTTP_URI; returns headers/body publisher for the configured writer", STREAM_RELAY, STREAM_RELAY, PRODUCE_RESULT),
                        mode("Body retries use the existing configured replay bound", BOUNDED_BUFFER, STREAM_RELAY, PRODUCE_RESULT)),
                        Set.of("http.result"), Set.of("http.uri", "http.writer"), Set.of(), null),
                descriptor("response", "response.ResponsePlugin", List.of(
                        mode("A matching configured HTTP MessageWriter consumes the selected transport result", NONE, STREAM_RELAY, CONSUME_RESULT, COMMIT)),
                        Set.of("http.writer"), Set.of("http.result"), Set.of("http-writer-binding"), null),
                descriptor("cache", "cache.CachePlugin", List.of(
                        mode("Eligible bodyless GET with a reusable hit writes a materialized cached response, making no upstream call", NONE, BUFFERED, SHORT_CIRCUIT, COMMIT),
                        mode("Eligible miss aggregates the whole body with DataBufferUtils.join before writing; no local aggregate cap is established", NONE, BUFFERED, DECORATE),
                        mode("Ineligible requests or the streaming flush path delegate to ordinary forwarding; cacheability does not imply a buffering bound", PASS_THROUGH, STREAM_RELAY)),
                        Set.of(), Set.of(), Set.of("response-cache-provider"), null));
    }

    private static PluginCapabilityDescriptor descriptor(final String plugin, final String implementation,
                                                         final List<PluginCapabilityDescriptor.Mode> modes, final Set<String> provides,
                                                         final Set<String> requires, final Set<String> supporting, final String configuration) {
        return new PluginCapabilityDescriptor(1, "1", plugin, "org.apache.shenyu.plugin." + implementation,
                Map.of("http", PluginCapabilityDescriptor.Support.DECLARED, "other", PluginCapabilityDescriptor.Support.UNKNOWN),
                modes, provides, requires, supporting, configuration, "built-in HTTP pilot; reviewed source 0b5e7c1df");
    }

    private static PluginCapabilityDescriptor.Mode mode(final String condition, final PluginCapabilityDescriptor.Body request,
                                                        final PluginCapabilityDescriptor.Body response,
                                                        final PluginCapabilityDescriptor.ResponseEffect... effects) {
        return new PluginCapabilityDescriptor.Mode(condition, request, response, Set.of(effects));
    }
}
