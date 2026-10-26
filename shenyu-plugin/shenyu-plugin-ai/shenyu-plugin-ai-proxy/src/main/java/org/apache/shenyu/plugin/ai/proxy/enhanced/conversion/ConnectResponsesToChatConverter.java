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

package org.apache.shenyu.plugin.ai.proxy.enhanced.conversion;

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
import io.github.agenticai.connect.ResponseStreamState;
import io.github.agenticai.connect.StreamOptions;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
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
 * Converts OpenAI Responses requests to Chat Completions and binds the reverse response conversion.
 */
public final class ConnectResponsesToChatConverter implements AiProtocolConverter {

    private static final Logger LOG = LoggerFactory.getLogger(ConnectResponsesToChatConverter.class);

    private static final ObjectMapper MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final Connect CONNECT = new Connect();

    private static final int MAX_CONVERSION_BYTES = 5 * 1024 * 1024;

    @Override
    public String sourceProtocol() {
        return OpenAiResponses.NAME;
    }

    @Override
    public String targetProtocol() {
        return OpenAiChat.NAME;
    }

    @Override
    public AiProtocolConversion convert(final ShenyuAiRequest request, final boolean stream) {
        Objects.requireNonNull(request, "AI request is required");
        if (!sourceProtocol().equals(request.getProtocol())) {
            throw new AiProxyException(AiProxyException.Kind.REQUEST,
                    "Expected OpenAI Responses request", null);
        }

        ConversionContext context = createContext(request);
        ObjectNode convertedPayload = convertRequest(request.getRawPayload(), context);
        ShenyuAiRequest convertedRequest = new OpenAiChat().decodeRequest(
                convertedPayload.toString(), convertedPayload);
        AiResponseAdapter responseAdapter = response -> convertResponse(response, stream, context);
        return new AiProtocolConversion(convertedRequest, responseAdapter);
    }

    private ConversionContext createContext(final ShenyuAiRequest request) {
        Options options = new Options();
        options.toolLossPolicy = Options.LossPolicy.SAFE;
        ConversionContext context = new ConversionContext(options);
        context.upstreamModel = Objects.requireNonNullElse(request.getModel(), "");
        return context;
    }

    private ObjectNode convertRequest(final ObjectNode payload, final ConversionContext context) {
        if (Objects.isNull(payload)) {
            throw new AiProxyException(AiProxyException.Kind.REQUEST, "AI request payload is required", null);
        }
        try {
            ConversionResult result = CONNECT.convertRequest(Format.OPENAI_RESPONSES, Format.OPENAI_CHAT,
                    Json.object(payload.toString()), context);
            reportDiagnostics(result.diagnostics());
            return (ObjectNode) MAPPER.readTree(result.json());
        } catch (Exception error) {
            if (error instanceof ConversionException conversionException) {
                reportDiagnostics(conversionException.diagnostics());
            }
            throw new AiProxyException(AiProxyException.Kind.REQUEST, "AI request protocol conversion failed", error);
        }
    }

    private AiUpstreamResponse convertResponse(final AiUpstreamResponse response, final boolean stream,
                                               final ConversionContext context) {
        if (response.getStatusCode() < 200 || response.getStatusCode() >= 300) {
            return response;
        }
        if (stream) {
            // Reuse ShenYu's incremental SSE framing and content-type validation, not a library HTTP client.
            AiUpstreamResponse framed = new OpenAiChat().encodeResponse(response, true);
            return new AiUpstreamResponse(response.getStatusCode(), responseHeaders(response, "text/event-stream"),
                    convertStream(framed.getBody(), context));
        }
        Flux<byte[]> body = response.getBody().reduceWith(ByteArrayOutputStream::new, (buffer, bytes) -> {
            if (bytes.length > MAX_CONVERSION_BYTES - buffer.size()) {
                throw new AiProxyException(AiProxyException.Kind.PROTOCOL,
                        "AI response exceeds the conversion size limit", null);
            }
            buffer.writeBytes(bytes);
            return buffer;
        }).map(buffer -> convertCompleteResponse(buffer.toString(StandardCharsets.UTF_8), context)).flux();
        return new AiUpstreamResponse(response.getStatusCode(), responseHeaders(response, "application/json"), body);
    }

    private byte[] convertCompleteResponse(final String body, final ConversionContext context) {
        try {
            JsonNode payload = MAPPER.readTree(body);
            if (Objects.isNull(payload) || !payload.isObject()) {
                throw new IllegalArgumentException("Expected an AI response object");
            }
            ConversionResult result = CONNECT.convertResponse(Format.OPENAI_CHAT, Format.OPENAI_RESPONSES,
                    Json.object(payload.toString()), context);
            reportDiagnostics(result.diagnostics());
            return result.json().getBytes(StandardCharsets.UTF_8);
        } catch (Exception error) {
            throw new AiProxyException(AiProxyException.Kind.PROTOCOL,
                    "AI response protocol conversion failed", error);
        }
    }

    private Flux<byte[]> convertStream(final Flux<byte[]> framed, final ConversionContext context) {
        return Flux.defer(() -> {
            StreamOptions options = new StreamOptions();
            options.emitSequenceNumber = true;
            ResponseStreamState state = CONNECT.newResponseStream(
                    Format.OPENAI_CHAT, Format.OPENAI_RESPONSES, options, context);
            int[] size = {0};
            return framed.concatMapIterable(bytes -> {
                // The framer produces exactly one validated event, retaining multiline JSON.
                String event = new String(bytes, StandardCharsets.UTF_8);
                String payload = event.substring(6, event.length() - 2).replace("\ndata: ", "\n");
                if ("[DONE]".equals(payload)) {
                    return List.<ConversionResult>of();
                }
                if (bytes.length > MAX_CONVERSION_BYTES - size[0]) {
                    throw new AiProxyException(AiProxyException.Kind.PROTOCOL,
                            "AI stream exceeds the conversion size limit", null);
                }
                size[0] += bytes.length;
                return state.accept(payload);
            }, 1).concatWith(Flux.defer(() -> {
                List<ConversionResult> terminal = state.finish();
                reportDiagnostics(state.diagnostics());
                return Flux.fromIterable(terminal);
            })).map(this::frameEvent)
                    .onErrorMap(error -> !(error instanceof AiProxyException),
                            error -> new AiProxyException(AiProxyException.Kind.PROTOCOL,
                                    "AI stream protocol conversion failed", error));
        });
    }

    private byte[] frameEvent(final ConversionResult result) {
        String type = result.value().get("type").getAsString();
        return ("event: " + type + "\ndata: " + result.json() + "\n\n").getBytes(StandardCharsets.UTF_8);
    }

    private Map<String, List<String>> responseHeaders(final AiUpstreamResponse response, final String contentType) {
        Map<String, List<String>> headers = new LinkedHashMap<>(response.getHeaders());
        headers.keySet().removeIf(name -> "Content-Length".equalsIgnoreCase(name)
                || "Content-Type".equalsIgnoreCase(name) || "ETag".equalsIgnoreCase(name)
                || "Content-MD5".equalsIgnoreCase(name) || "Digest".equalsIgnoreCase(name));
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
