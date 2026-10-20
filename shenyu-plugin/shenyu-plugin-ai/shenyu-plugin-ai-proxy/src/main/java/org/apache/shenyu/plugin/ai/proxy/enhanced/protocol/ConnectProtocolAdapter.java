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

package org.apache.shenyu.plugin.ai.proxy.enhanced.protocol;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.github.agenticai.connect.Connect;
import io.github.agenticai.connect.ConversionContext;
import io.github.agenticai.connect.ConversionException;
import io.github.agenticai.connect.ConversionResult;
import io.github.agenticai.connect.Diagnostic;
import io.github.agenticai.connect.Format;
import io.github.agenticai.connect.Json;
import io.github.agenticai.connect.Options;
import io.github.agenticai.connect.ProviderProfile;
import io.github.agenticai.connect.ResponseStreamState;
import io.github.agenticai.connect.StreamOptions;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.Flux;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Request-scoped bridge between ShenYu's reactive HTTP boundary and Connect's JSON codecs.
 * No transport, routing, credentials, retry or billing policy is delegated to Connect.
 */
public final class ConnectProtocolAdapter {

    private static final Logger LOG = LoggerFactory.getLogger(ConnectProtocolAdapter.class);

    private static final ObjectMapper MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final Connect CONNECT = new Connect();

    private static final int MAX_CONVERSION_BYTES = 5 * 1024 * 1024;

    private final Format clientFormat;

    private final ConversionContext context;

    /**
     * Create an isolated context for a request and its response, never a singleton context.
     *
     * @param protocol client protocol identifier
     * @param upstreamModel authoritative routed model, or null to retain the client model
     * @param profile explicitly selected provider dialect, or null for generic Chat
     */
    public ConnectProtocolAdapter(final String protocol, final String upstreamModel, final ProviderProfile profile) {
        clientFormat = OpenAiResponses.NAME.equals(protocol) ? Format.OPENAI_RESPONSES : Format.OPENAI_CHAT;
        Options options = new Options();
        options.toolLossPolicy = Options.LossPolicy.SAFE;
        options.providerProfile = profile;
        context = new ConversionContext(options);
        context.upstreamModel = Objects.isNull(upstreamModel) ? "" : upstreamModel;
    }

    /**
     * Convert the final gateway-configured payload to Chat and apply the provider profile once.
     *
     * @param payload copied client payload with gateway overrides applied
     * @return converted payload
     */
    public ObjectNode convertRequest(final ObjectNode payload) {
        try {
            ConversionResult result = CONNECT.convertRequest(clientFormat, Format.OPENAI_CHAT, Json.object(payload.toString()), context);
            reportDiagnostics(result.diagnostics());
            return (ObjectNode) MAPPER.readTree(result.json());
        } catch (Exception error) {
            if (error instanceof ConversionException) {
                reportDiagnostics(((ConversionException) error).diagnostics());
            }
            throw new AiProxyException(AiProxyException.Kind.REQUEST, "AI request protocol conversion failed", error);
        }
    }

    /**
     * Convert successful Responses bodies; preserve Chat and upstream HTTP errors unchanged.
     *
     * @param response upstream response with an unconsumed body
     * @param stream effective streaming mode
     * @return adapted response without subscribing or blocking
     */
    public AiUpstreamResponse convertResponse(final AiUpstreamResponse response, final boolean stream) {
        if (clientFormat == Format.OPENAI_CHAT || response.getStatusCode() < 200 || response.getStatusCode() >= 300) {
            return response;
        }
        if (stream) {
            // Reuse ShenYu's incremental SSE framing and content-type validation, not a library HTTP client.
            AiUpstreamResponse framed = new OpenAiChat().encodeResponse(response, true);
            return new AiUpstreamResponse(response.getStatusCode(), responseHeaders(response, "text/event-stream"), convertStream(framed.getBody()));
        }
        Flux<byte[]> body = response.getBody().reduceWith(ByteArrayOutputStream::new, (buffer, bytes) -> {
            if (bytes.length > MAX_CONVERSION_BYTES - buffer.size()) {
                throw new AiProxyException(AiProxyException.Kind.PROTOCOL, "AI response exceeds the conversion size limit", null);
            }
            buffer.writeBytes(bytes);
            return buffer;
        }).map(buffer -> convertCompleteResponse(buffer.toString(StandardCharsets.UTF_8))).flux();
        return new AiUpstreamResponse(response.getStatusCode(), responseHeaders(response, "application/json"), body);
    }

    private byte[] convertCompleteResponse(final String body) {
        try {
            JsonNode payload = MAPPER.readTree(body);
            if (Objects.isNull(payload) || !payload.isObject()) {
                throw new IllegalArgumentException("Expected an AI response object");
            }
            ConversionResult result = CONNECT.convertResponse(Format.OPENAI_CHAT, clientFormat, Json.object(payload.toString()), context);
            reportDiagnostics(result.diagnostics());
            return result.json().getBytes(StandardCharsets.UTF_8);
        } catch (Exception error) {
            throw new AiProxyException(AiProxyException.Kind.PROTOCOL, "AI response protocol conversion failed", error);
        }
    }

    private Flux<byte[]> convertStream(final Flux<byte[]> framed) {
        return Flux.defer(() -> {
            StreamOptions options = new StreamOptions();
            options.emitSequenceNumber = true;
            ResponseStreamState state = CONNECT.newResponseStream(Format.OPENAI_CHAT, clientFormat, options, context);
            int[] size = {0};
            return framed.concatMapIterable(bytes -> {
                // The framer produces exactly one validated event, retaining multiline JSON.
                String event = new String(bytes, StandardCharsets.UTF_8);
                String payload = event.substring(6, event.length() - 2).replace("\ndata: ", "\n");
                if ("[DONE]".equals(payload)) {
                    return List.<ConversionResult>of();
                }
                if (bytes.length > MAX_CONVERSION_BYTES - size[0]) {
                    throw new AiProxyException(AiProxyException.Kind.PROTOCOL, "AI stream exceeds the conversion size limit", null);
                }
                size[0] += bytes.length;
                return state.accept(payload);
            }, 1).concatWith(Flux.defer(() -> {
                List<ConversionResult> terminal = state.finish();
                reportDiagnostics(state.diagnostics());
                return Flux.fromIterable(terminal);
            })).map(this::frameEvent)
                    .onErrorMap(error -> !(error instanceof AiProxyException),
                            error -> new AiProxyException(AiProxyException.Kind.PROTOCOL, "AI stream protocol conversion failed", error));
        });
    }

    private byte[] frameEvent(final ConversionResult result) {
        String type = result.value().get("type").getAsString();
        return ("event: " + type + "\ndata: " + result.json() + "\n\n").getBytes(StandardCharsets.UTF_8);
    }

    private Map<String, List<String>> responseHeaders(final AiUpstreamResponse response, final String contentType) {
        Map<String, List<String>> headers = new LinkedHashMap<>(response.getHeaders());
        headers.keySet().removeIf(name -> "Content-Length".equalsIgnoreCase(name) || "Content-Type".equalsIgnoreCase(name)
                || "ETag".equalsIgnoreCase(name) || "Content-MD5".equalsIgnoreCase(name) || "Digest".equalsIgnoreCase(name));
        headers.put("Content-Type", List.of(contentType));
        return headers;
    }

    private void reportDiagnostics(final List<Diagnostic> diagnostics) {
        for (Diagnostic diagnostic : diagnostics) {
            // Never log payloads, credentials, private reasoning or library diagnostic messages.
            LOG.warn("AI conversion diagnostic: code={}, severity={}, from={}, to={}",
                    diagnostic.code(), diagnostic.severity(), diagnostic.from(), diagnostic.to());
        }
    }
}
