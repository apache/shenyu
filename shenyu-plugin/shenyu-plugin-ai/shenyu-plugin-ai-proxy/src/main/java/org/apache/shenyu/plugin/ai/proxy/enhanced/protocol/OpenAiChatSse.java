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
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.common.metrics.AiStreamCompletion;
import reactor.core.publisher.Flux;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Incremental OpenAI Chat SSE framing. Only the current event is buffered.
 */
final class OpenAiChatSse {

    private static final ObjectMapper MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final int MAX_EVENT_BYTES = 5 * 1024 * 1024;

    private final ByteArrayOutputStream line = new ByteArrayOutputStream();

    private final StringBuilder data = new StringBuilder();

    private boolean carriageReturn;

    private boolean done;

    private int eventBytes;

    private final AiStreamCompletionProperties.Mode mode;

    private final AiRequestObservation.Attempt observation;

    private OpenAiChatSse(final AiStreamCompletionProperties.Mode mode, final AiRequestObservation.Attempt observation) {
        this.mode = mode;
        this.observation = observation;
    }

    static Flux<byte[]> encode(final Flux<byte[]> source) {
        return encode(source, AiStreamCompletionProperties.Mode.COMPATIBILITY);
    }

    static Flux<byte[]> encode(final Flux<byte[]> source, final AiStreamCompletionProperties.Mode mode) {
        return Flux.deferContextual(context -> {
            OpenAiChatSse decoder = new OpenAiChatSse(mode, context.getOrDefault(AiRequestObservation.Attempt.REACTOR_KEY, null));
            decoder.framing(AiStreamCompletion.Framing.NONE);
            return source.concatMapIterable(decoder::accept, 1)
                    .takeUntil(bytes -> java.util.Arrays.equals(bytes, "data: [DONE]\n\n".getBytes(StandardCharsets.UTF_8)))
                    .concatWith(Flux.defer(() -> Flux.fromIterable(decoder.finish())))
                    .doOnError(ignored -> decoder.framing(AiStreamCompletion.Framing.NONE))
                    .doOnCancel(() -> {
                        if (!decoder.done) {
                            decoder.framing(AiStreamCompletion.Framing.NONE);
                        }
                    });
        });
    }

    private Iterable<byte[]> accept(final byte[] bytes) {
        return () -> new EventIterator(bytes);
    }

    private void readLine(final List<byte[]> events) {
        String text = line.toString(StandardCharsets.UTF_8);
        line.reset();
        if (text.isEmpty()) {
            dispatch(events);
            eventBytes = 0;
        } else if ("data".equals(text) || text.startsWith("data:")) {
            String value = "data".equals(text) ? "" : text.substring(5);
            if (value.startsWith(" ")) {
                value = value.substring(1);
            }
            data.append(value).append('\n');
        }
    }

    private void dispatch(final List<byte[]> events) {
        if (data.isEmpty()) {
            return;
        }
        String payload = data.substring(0, data.length() - 1);
        data.setLength(0);
        if ("[DONE]".equals(payload)) {
            done = true;
            framing(AiStreamCompletion.Framing.PASSTHROUGH_MARKER);
        } else {
            try {
                JsonNode parsed = MAPPER.readTree(payload);
                if (!parsed.isObject()) {
                    throw new IllegalArgumentException("Expected a JSON object");
                }
            } catch (Exception error) {
                throw new AiProxyException(AiProxyException.Kind.PROTOCOL, "Malformed AI stream event", error);
            }
        }
        // Use one data line per payload line to retain valid SSE framing for multiline JSON.
        events.add(("data: " + payload.replace("\n", "\ndata: ") + "\n\n").getBytes(StandardCharsets.UTF_8));
    }

    private List<byte[]> finish() {
        List<byte[]> events = new ArrayList<>();
        if (!done) {
            if (mode == AiStreamCompletionProperties.Mode.STRICT) {
                framing(AiStreamCompletion.Framing.NONE);
                throw new AiProxyException(AiProxyException.Kind.PROTOCOL, "AI stream ended without a terminal marker", null);
            }
            if (line.size() > 0) {
                readLine(events);
            }
            dispatch(events);
            if (!done) {
                framing(AiStreamCompletion.Framing.SYNTHESIZED_MARKER);
                events.add("data: [DONE]\n\n".getBytes(StandardCharsets.UTF_8));
            }
        }
        return events;
    }

    private void framing(final AiStreamCompletion.Framing value) {
        if (java.util.Objects.nonNull(observation)) {
            observation.streamFraming(value);
        }
    }

    private final class EventIterator implements Iterator<byte[]> {

        private final byte[] bytes;

        private int index;

        private byte[] next;

        private EventIterator(final byte[] bytes) {
            this.bytes = bytes;
        }

        @Override
        public boolean hasNext() {
            if (java.util.Objects.nonNull(next)) {
                return true;
            }
            List<byte[]> events = new ArrayList<>(1);
            while (index < bytes.length && !done) {
                byte value = bytes[index++];
                if (carriageReturn && value == '\n') {
                    carriageReturn = false;
                    continue;
                }
                carriageReturn = false;
                if (++eventBytes > MAX_EVENT_BYTES) {
                    throw new AiProxyException(AiProxyException.Kind.PROTOCOL, "AI stream event exceeds the size limit", null);
                }
                if (value == '\r' || value == '\n') {
                    readLine(events);
                    carriageReturn = value == '\r';
                    if (!events.isEmpty()) {
                        next = events.get(0);
                        return true;
                    }
                } else {
                    line.write(value);
                }
            }
            return false;
        }

        @Override
        public byte[] next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            byte[] event = next;
            next = null;
            return event;
        }
    }
}
