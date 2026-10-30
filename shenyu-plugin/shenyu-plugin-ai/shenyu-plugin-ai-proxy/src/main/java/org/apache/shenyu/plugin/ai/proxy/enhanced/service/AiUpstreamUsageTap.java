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

    private final ByteArrayOutputStream line = new ByteArrayOutputStream();

    private final StringBuilder event = new StringBuilder();

    private boolean carriageReturn;

    private boolean stopped;

    private int eventBytes;

    AiUpstreamUsageTap(final AiRequestObservation.Attempt observation, final Map<String, List<String>> headers, final boolean stream) {
        this.observation = observation;
        String contentType = header(headers, "Content-Type").toLowerCase(Locale.ROOT).split(";", 2)[0].trim();
        streaming = contentType.isEmpty() ? stream : "text/event-stream".equals(contentType);
        String encoding = header(headers, "Content-Encoding");
        if (!encoding.isEmpty() && !"identity".equalsIgnoreCase(encoding)
                || !contentType.isEmpty() && !"text/event-stream".equals(contentType) && !"application/json".equals(contentType) && !contentType.endsWith("+json")) {
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
        JsonNode usage = root.get("usage");
        if (Objects.isNull(usage) || usage.isNull()) {
            return;
        }
        if (!usage.isObject()) {
            observation.usage(new AiUsage(null, null, null, AiUsage.Status.INVALID));
            return;
        }
        Long prompt = count(usage.get("prompt_tokens"));
        Long completion = count(usage.get("completion_tokens"));
        Long total = count(usage.get("total_tokens"));
        AiUsage.Status quality = invalid(usage.get("prompt_tokens")) || invalid(usage.get("completion_tokens")) || invalid(usage.get("total_tokens"))
                ? AiUsage.Status.INVALID : Objects.isNull(prompt) && Objects.isNull(completion) && Objects.isNull(total) ? AiUsage.Status.UNKNOWN : AiUsage.Status.REPORTED;
        // Replace the snapshot, never sum cumulative SSE counters or fill fields from an older event.
        observation.usage(quality == AiUsage.Status.INVALID ? new AiUsage(null, null, null, quality) : new AiUsage(prompt, completion, total, quality));
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
