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

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

/**
 * Bounded raw upstream side tap. Never subscribes, changes bytes, estimates tokens,
 * logs payloads or treats protocol-conversion defaults as provider-reported usage.
 */
final class AiUpstreamUsageTap {

    static final int MAX_BYTES = 64 * 1024;

    private static final ObjectMapper MAPPER = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private final AiRequestObservation.Attempt observation;

    private final boolean streaming;

    private final String protocol;

    private AiUsage snapshot = AiUsage.UNKNOWN;

    private final ByteArrayOutputStream line = new ByteArrayOutputStream();

    private final StringBuilder event = new StringBuilder();

    private boolean carriageReturn;

    private boolean stopped;

    private int eventBytes;

    AiUpstreamUsageTap(final AiRequestObservation.Attempt observation, final Map<String, List<String>> headers, final boolean stream) {
        this(observation, headers, stream, "openai-chat");
    }

    AiUpstreamUsageTap(final AiRequestObservation.Attempt observation, final Map<String, List<String>> headers, final boolean stream, final String protocol) {
        this.observation = observation;
        this.protocol = protocol;
        String contentType = header(headers, "Content-Type").toLowerCase(Locale.ROOT).split(";", 2)[0].trim();
        streaming = contentType.isEmpty() ? stream : "text/event-stream".equals(contentType);
        String encoding = header(headers, "Content-Encoding");
        if (!encoding.isEmpty() && !"identity".equalsIgnoreCase(encoding)
                || !contentType.isEmpty() && !"text/event-stream".equals(contentType) && !"application/json".equals(contentType) && !contentType.endsWith("+json")) {
            unavailable(AiUsage.Status.UNSUPPORTED);
        }
        if (!List.of("openai-chat", "openai-responses", "anthropic-messages").contains(protocol)) {
            unavailable(AiUsage.Status.UNSUPPORTED);
        }
    }

    AiUpstreamResponse attach(final AiUpstreamResponse response) {
        return new AiUpstreamResponse(response.getStatusCode(), response.getHeaders(), response.getBody()
                .doOnNext(this::accept).doOnComplete(this::complete).doFinally(ignored -> clear()));
    }

    synchronized void accept(final byte[] bytes) {
        if (stopped) {
            return;
        }
        try {
            if (!streaming) {
                if (bytes.length > MAX_BYTES - line.size()) {
                    unavailable(AiUsage.Status.LIMIT_EXCEEDED);
                } else {
                    line.writeBytes(bytes);
                }
                return;
            }
            for (byte value : bytes) {
                if (stopped) {
                    break;
                }
                if (carriageReturn && value == '\n') {
                    carriageReturn = false;
                    continue;
                }
                carriageReturn = false;
                if (++eventBytes > MAX_BYTES) {
                    unavailable(AiUsage.Status.LIMIT_EXCEEDED);
                    break;
                }
                if (value == '\r' || value == '\n') {
                    readLine();
                    carriageReturn = value == '\r';
                } else {
                    line.write(value);
                }
            }
        } catch (Exception ignored) {
            unavailable(AiUsage.Status.INVALID);
        }
    }

    private void readLine() throws java.io.IOException {
        String text = StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT).onUnmappableCharacter(CodingErrorAction.REPORT)
                .decode(ByteBuffer.wrap(line.toByteArray())).toString();
        line.reset();
        if (text.isEmpty()) {
            dispatch();
            eventBytes = 0;
        } else if ("data".equals(text) || text.startsWith("data:")) {
            String value = "data".equals(text) ? "" : text.substring(5);
            event.append(value.startsWith(" ") ? value.substring(1) : value).append('\n');
        }
    }

    private void dispatch() throws java.io.IOException {
        if (event.isEmpty()) {
            return;
        }
        String payload = event.substring(0, event.length() - 1);
        event.setLength(0);
        if ("[DONE]".equals(payload)) {
            stopped = true;
        } else {
            capture(MAPPER.readTree(payload));
        }
    }

    synchronized void complete() {
        if (stopped) {
            return;
        }
        try {
            if (!streaming && line.size() > 0) {
                capture(MAPPER.readTree(line.toByteArray()));
            }
        } catch (Exception ignored) {
            unavailable(AiUsage.Status.INVALID);
        } finally {
            clear();
        }
    }

    private void capture(final JsonNode root) {
        if (Objects.isNull(root) || !root.isObject()) {
            unavailable(AiUsage.Status.INVALID);
            return;
        }
        JsonNode envelope = root;
        boolean patch = false;
        if ("openai-responses".equals(protocol) && streaming) {
            if (!List.of("response.completed", "response.incomplete", "response.failed").contains(root.path("type").asText())) {
                return;
            }
            envelope = root.path("response");
        } else if ("anthropic-messages".equals(protocol) && streaming) {
            String type = root.path("type").asText();
            if ("message_start".equals(type)) {
                envelope = root.path("message");
            } else if ("message_delta".equals(type)) {
                patch = true;
            } else {
                return;
            }
        }
        JsonNode usage = envelope.get("usage");
        if (Objects.isNull(usage) || usage.isNull()) {
            if ("openai-responses".equals(protocol) || "anthropic-messages".equals(protocol) && !patch) {
                snapshot = AiUsage.UNKNOWN;
                observation.usage(snapshot);
            }
            return;
        }
        if (!usage.isObject()) {
            unavailable(AiUsage.Status.INVALID);
            return;
        }
        captureUsage(usage, patch);
    }

    private void captureUsage(final JsonNode usage, final boolean patch) {
        boolean chat = "openai-chat".equals(protocol);
        String input = chat ? "prompt_tokens" : "input_tokens";
        String output = chat ? "completion_tokens" : "output_tokens";
        String details = chat ? "prompt_tokens_details" : "input_tokens_details";
        for (String field : new String[]{input, output, "total_tokens", "cache_read_input_tokens", "cache_creation_input_tokens", "cached_tokens", "prompt_cache_hit_tokens"}) {
            if (invalid(usage.get(field))) {
                unavailable(AiUsage.Status.INVALID);
                return;
            }
        }
        JsonNode breakdown = usage.get(details);
        if (Objects.nonNull(breakdown) && !breakdown.isNull() && (!breakdown.isObject() || invalid(breakdown.get("cached_tokens")))) {
            unavailable(AiUsage.Status.INVALID);
            return;
        }
        Map<String, Long> categories = categories(usage, breakdown, patch);
        if (Objects.isNull(categories)) {
            return;
        }
        Long prompt = patch && !usage.has(input) ? snapshot.promptTokens() : count(usage.get(input));
        Long completion = patch && !usage.has(output) ? snapshot.completionTokens() : count(usage.get(output));
        Long total = patch && !usage.has("total_tokens") ? snapshot.totalTokens() : count(usage.get("total_tokens"));
        Long cached = "anthropic-messages".equals(protocol) ? patch && !usage.has("cache_read_input_tokens")
                ? snapshot.cachedPromptTokens() : count(usage.get("cache_read_input_tokens")) : cached(usage, details);
        AiUsage.Status quality = Objects.isNull(prompt) && Objects.isNull(completion) && Objects.isNull(total)
                && Objects.isNull(cached) && categories.isEmpty() ? AiUsage.Status.UNKNOWN : AiUsage.Status.REPORTED;
        // OpenAI snapshots replace. Anthropic sparse cumulative patches replace present fields, never add counters.
        snapshot = new AiUsage(prompt, completion, total, cached, categories, quality);
        observation.usage(snapshot);
    }

    private Map<String, Long> categories(final JsonNode usage, final JsonNode breakdown, final boolean patch) {
        Map<String, Long> categories = patch ? new LinkedHashMap<>(snapshot.providerCounts()) : new LinkedHashMap<>();
        for (String field : new String[]{"cache_creation_input_tokens"}) {
            if (usage.has(field)) {
                categories.remove(field);
                Long value = count(usage.get(field));
                if (Objects.nonNull(value)) {
                    categories.put(field, value);
                }
            }
        }
        // Other billable provider units remain visible; a fixed input/output price cannot silently absorb them.
        JsonNode serverTools = usage.get("server_tool_use");
        if (Objects.nonNull(serverTools) && !serverTools.isNull()) {
            if (!serverTools.isObject()) {
                unavailable(AiUsage.Status.INVALID);
                return null;
            }
            var fields = serverTools.fields();
            while (fields.hasNext()) {
                var field = fields.next();
                if (!List.of("web_search_requests", "web_fetch_requests", "code_execution_requests").contains(field.getKey())) {
                    unavailable(AiUsage.Status.UNSUPPORTED);
                    return null;
                }
                if (invalid(field.getValue()) || categories.size() >= 32) {
                    unavailable(AiUsage.Status.INVALID);
                    return null;
                }
                if (Objects.nonNull(count(field.getValue()))) {
                    categories.put("server_tool_use." + field.getKey(), count(field.getValue()));
                }
            }
        }
        JsonNode cacheCreation = usage.get("cache_creation");
        if (Objects.nonNull(cacheCreation) && !cacheCreation.isNull()) {
            if (!cacheCreation.isObject()) {
                unavailable(AiUsage.Status.INVALID);
                return null;
            }
            for (String field : List.of("ephemeral_5m_input_tokens", "ephemeral_1h_input_tokens")) {
                if (invalid(cacheCreation.get(field))) {
                    unavailable(AiUsage.Status.INVALID);
                    return null;
                }
                if (Objects.nonNull(count(cacheCreation.get(field)))) {
                    categories.put("cache_creation." + field, count(cacheCreation.get(field)));
                }
            }
        }
        if (Objects.nonNull(breakdown) && invalid(breakdown.get("cache_write_tokens"))) {
            unavailable(AiUsage.Status.INVALID);
            return null;
        }
        if (Objects.nonNull(breakdown) && Objects.nonNull(count(breakdown.get("cache_write_tokens")))) {
            categories.put("cache_write_tokens", count(breakdown.get("cache_write_tokens")));
        }
        return extras(usage, categories);
    }

    private Map<String, Long> extras(final JsonNode usage, final Map<String, Long> categories) {
        var extra = usage.fields();
        while (extra.hasNext()) {
            var field = extra.next();
            if (field.getKey().endsWith("_tokens") && !List.of("prompt_tokens", "completion_tokens", "input_tokens", "output_tokens", "total_tokens",
                    "cached_tokens", "prompt_cache_hit_tokens", "cache_read_input_tokens", "cache_creation_input_tokens").contains(field.getKey())) {
                if (invalid(field.getValue()) || field.getKey().length() > 128 || categories.size() >= 32) {
                    unavailable(AiUsage.Status.INVALID);
                    return null;
                }
                if (Objects.nonNull(count(field.getValue()))) {
                    categories.put(field.getKey(), count(field.getValue()));
                }
            }
        }
        return categories;
    }

    private Long cached(final JsonNode usage, final String details) {
        for (JsonNode value : new JsonNode[]{usage.path(details).get("cached_tokens"), usage.get("cached_tokens"), usage.get("prompt_cache_hit_tokens")}) {
            Long result = count(value);
            if (Objects.nonNull(result)) {
                return result;
            }
        }
        return null;
    }

    private Long count(final JsonNode node) {
        return Objects.nonNull(node) && node.isIntegralNumber() && node.canConvertToLong() && node.longValue() >= 0 ? node.longValue() : null;
    }

    private boolean invalid(final JsonNode node) {
        return Objects.nonNull(node) && !node.isNull() && Objects.isNull(count(node));
    }

    private void unavailable(final AiUsage.Status status) {
        observation.usage(new AiUsage(null, null, null, status));
        clear();
    }

    private synchronized void clear() {
        stopped = true;
        line.reset();
        event.setLength(0);
    }

    private String header(final Map<String, List<String>> headers, final String key) {
        return headers.entrySet().stream().filter(entry -> key.equalsIgnoreCase(entry.getKey()))
                .flatMap(entry -> entry.getValue().stream()).findFirst().orElse("");
    }
}
