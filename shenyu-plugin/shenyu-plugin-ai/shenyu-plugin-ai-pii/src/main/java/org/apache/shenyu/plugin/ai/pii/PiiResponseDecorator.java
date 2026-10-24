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

package org.apache.shenyu.plugin.ai.pii;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.plugin.ai.pii.audit.PiiAuditSink;
import org.apache.shenyu.plugin.ai.pii.detector.PiiDetectorTimeoutException;
import org.apache.shenyu.plugin.ai.pii.handler.PiiDetectionRule;
import org.apache.shenyu.plugin.ai.pii.profile.PiiProtocolProfile;
import org.apache.shenyu.plugin.ai.pii.profile.PiiProtocolProfiles;
import org.apache.shenyu.plugin.ai.pii.profile.PiiTextSlot;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

/**
 * The response side of the pii detection plugin.
 *
 * <p>The AI proxy writes its responses through {@code writeAndFlushWith}, so a
 * decorator that only overrides {@code writeWith} would never see the body:
 * both methods are overridden here. Server sent events and compressed bodies
 * are passed through unscanned with a warning, they are unsupported
 * enforcement modes of this release, and a response the request side already
 * marked as skipped is passed through as well. The scan and the re
 * serialization run on a bounded elastic worker, never on the netty event
 * loop.</p>
 */
final class PiiResponseDecorator extends ServerHttpResponseDecorator {

    private static final Logger LOG = LoggerFactory.getLogger(PiiResponseDecorator.class);

    private static final String BLOCK_BODY = "{\"error\":{\"message\":\"Response rejected:"
            + " sensitive content detected\",\"type\":\"pii_detection_error\"}}";

    private final ServerWebExchange exchange;

    private final PiiDetectionRule rule;

    private final PiiAuditSink auditSink;

    PiiResponseDecorator(final ServerWebExchange exchange, final PiiDetectionRule rule, final PiiAuditSink auditSink) {
        super(exchange.getResponse());
        this.exchange = exchange;
        this.rule = rule;
        this.auditSink = auditSink;
    }

    @Override
    @NonNull
    public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
        if (skipScan()) {
            return super.writeWith(body);
        }
        long declaredSize = getHeaders().getContentLength();
        if (rule.isOversized(declaredSize)) {
            if (rule.isFailClosed()) {
                LOG.warn("pii detection: the response of {} bytes exceeds maxBodySize {}, failClosed is set,"
                        + " the response is rejected", declaredSize, rule.getMaxBodySize());
                return Mono.fromCallable(this::block)
                        .subscribeOn(Schedulers.boundedElastic())
                        .flatMap(this::write);
            }
            // the body is never buffered on this path, it streams through unchanged
            LOG.warn("pii detection: the response of {} bytes exceeds maxBodySize {}, the response is not scanned,"
                    + " ruleId {}", declaredSize, rule.getMaxBodySize(), rule.getRuleId());
            return super.writeWith(body);
        }
        return DataBufferUtils.join(body)
                .publishOn(Schedulers.boundedElastic())
                .flatMap(buffer -> {
                    byte[] bytes = new byte[buffer.readableByteCount()];
                    buffer.read(bytes);
                    DataBufferUtils.release(buffer);
                    return Mono.just(scan(bytes));
                })
                .flatMap(this::write)
                .switchIfEmpty(Mono.defer(() -> super.writeWith(Flux.empty())));
    }

    @Override
    @NonNull
    public Mono<Void> writeAndFlushWith(@NonNull final Publisher<? extends Publisher<? extends DataBuffer>> body) {
        if (skipScan()) {
            return super.writeAndFlushWith(body);
        }
        if (!scannableContentType()) {
            // server sent events and bodies without a json content type are unsupported
            // enforcement modes: a fail closed rule rejects them, a fail open rule
            // streams them through unchanged instead of joining them
            if (rule.isFailClosed()) {
                LOG.warn("pii detection: the response content type {} cannot be scanned and failClosed is set,"
                        + " the response is rejected", getHeaders().getContentType());
                return Mono.fromCallable(this::block)
                        .subscribeOn(Schedulers.boundedElastic())
                        .flatMap(this::write);
            }
            LOG.warn("pii detection: the response content type {} cannot be scanned, the response is passed"
                    + " through unscanned, ruleId {}", getHeaders().getContentType(), rule.getRuleId());
            return super.writeAndFlushWith(body);
        }
        return writeWith(Flux.from(body).concatMap(Flux::from));
    }

    private byte[] scan(final byte[] bytes) {
        if (rule.isOversized(bytes.length)) {
            if (rule.isFailClosed()) {
                LOG.warn("pii detection: the response of {} bytes exceeds maxBodySize {}, failClosed is set,"
                        + " the response is rejected", bytes.length, rule.getMaxBodySize());
                return block();
            }
            LOG.warn("pii detection: the response of {} bytes exceeds maxBodySize {}, the response is not scanned,"
                    + " ruleId {}", bytes.length, rule.getMaxBodySize(), rule.getRuleId());
            return bytes;
        }
        MediaType contentType = getHeaders().getContentType();
        String encoding = getHeaders().getFirst(HttpHeaders.CONTENT_ENCODING);
        if (Objects.nonNull(encoding) && !encoding.isEmpty() && !"identity".equalsIgnoreCase(encoding)) {
            return passThroughUnsupported(new String(bytes, StandardCharsets.UTF_8),
                    "the response carries Content-Encoding " + encoding);
        }
        if (!scannableContentType()) {
            return passThroughUnsupported(new String(bytes, StandardCharsets.UTF_8),
                    "the response has no scannable json content type");
        }
        String body = new String(bytes, StandardCharsets.UTF_8);
        JsonObject root;
        try {
            JsonElement element = JsonParser.parseString(body);
            root = element.isJsonObject() ? element.getAsJsonObject() : null;
        } catch (RuntimeException e) {
            root = null;
        }
        if (Objects.isNull(root)) {
            return passThroughUnsupported(body, "the response body is not a JSON object");
        }
        if (root.has("error")) {
            // an upstream error envelope is never rewritten, whatever the failure policy
            return bytes;
        }
        PiiProtocolProfile profile = PiiProtocolProfiles.selectResponse(root);
        if (Objects.isNull(profile)) {
            return passThroughUnsupported(body, "no supported protocol profile matches the response");
        }
        List<PiiTextSlot> slots;
        PiiScanner.PiiOutcome outcome;
        try {
            slots = profile.responseSlots(root);
            outcome = new PiiScanner(rule, "response").scan(slots);
        } catch (PiiDetectorTimeoutException e) {
            return passThroughUnsupported(body, e.getMessage());
        }
        outcome.getAudits().forEach(auditSink::record);
        if (outcome.isBlocked()) {
            LOG.warn("pii detection: the response was rejected, ruleId {}", rule.getRuleId());
            return block();
        }
        return outcome.isChanged() ? root.toString().getBytes(StandardCharsets.UTF_8) : bytes;
    }

    private byte[] passThroughUnsupported(final String body, final String reason) {
        if (rule.isFailClosed()) {
            LOG.warn("pii detection: {} and failClosed is set, the response is rejected", reason);
            return block();
        }
        LOG.warn("pii detection: {}, the response is passed through unscanned", reason);
        return body.getBytes(StandardCharsets.UTF_8);
    }

    private byte[] block() {
        getHeaders().remove(HttpHeaders.CONTENT_LENGTH);
        getHeaders().remove(HttpHeaders.CONTENT_ENCODING);
        getHeaders().setContentType(MediaType.APPLICATION_JSON);
        setStatusCode(HttpStatus.FORBIDDEN);
        return BLOCK_BODY.getBytes(StandardCharsets.UTF_8);
    }

    private Mono<Void> write(final byte[] bytes) {
        getHeaders().setContentLength(bytes.length);
        return super.writeWith(Mono.just(bufferFactory().wrap(bytes)));
    }

    private boolean skipScan() {
        return Boolean.TRUE.equals(exchange.getAttribute(PiiDetectionPlugin.SKIP_RESPONSE_ATTRIBUTE));
    }

    private boolean scannableContentType() {
        MediaType contentType = getHeaders().getContentType();
        if (Objects.isNull(contentType)) {
            return false;
        }
        if (contentType.isCompatibleWith(MediaType.TEXT_EVENT_STREAM)) {
            return false;
        }
        if (contentType.isCompatibleWith(MediaType.APPLICATION_JSON)) {
            return true;
        }
        String subtype = contentType.getSubtype();
        return Objects.nonNull(subtype) && subtype.endsWith("json");
    }
}
