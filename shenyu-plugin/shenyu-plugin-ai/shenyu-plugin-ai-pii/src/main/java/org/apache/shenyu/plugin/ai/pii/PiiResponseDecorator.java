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

import java.io.ByteArrayOutputStream;
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

    /**
     * Whether the aggregated body exceeded the bound of the rule. The decorator
     * instance serves a single response, so the flag needs no synchronization.
     */
    private boolean aggregatedOverLimit;

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
        return aggregate(body)
                .publishOn(Schedulers.boundedElastic())
                .map(bytes -> aggregatedOverLimit ? passThroughUnscanned(bytes) : scan(bytes))
                .onErrorResume(ResponseTooLargeException.class, tooLarge -> {
                    LOG.warn("pii detection: the response exceeds maxBodySize {} and failClosed is set,"
                            + " the reading stopped and the response is rejected", rule.getMaxBodySize());
                    return Mono.just(block());
                })
                .flatMap(this::write)
                .switchIfEmpty(Mono.defer(() -> super.writeWith(Flux.empty())));
    }

    /**
     * Aggregate the body under the size bound of the rule. A fail closed rule
     * stops reading as soon as the bound is exceeded, the upstream is cancelled
     * and the buffers read so far are released. A fail open rule keeps reading
     * to the end, because passing a truncated body through would corrupt the
     * response: the complete body is returned with {@link #aggregatedOverLimit}
     * set and the caller writes it back unscanned.
     *
     * @param body the body publisher
     * @return the aggregated bytes
     */
    private Mono<byte[]> aggregate(final Publisher<? extends DataBuffer> body) {
        final long limit = rule.getMaxBodySize();
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        return Flux.from(body)
                .doOnNext(buffer -> copyInto(buffer, out, limit))
                .then(Mono.fromCallable(out::toByteArray));
    }

    private void copyInto(final DataBuffer buffer, final ByteArrayOutputStream out, final long limit) {
        final int size = buffer.readableByteCount();
        try {
            if (limit > 0L && !aggregatedOverLimit && out.size() + size > limit) {
                if (rule.isFailClosed()) {
                    throw new ResponseTooLargeException();
                }
                aggregatedOverLimit = true;
                LOG.warn("pii detection: the response exceeds maxBodySize {}, the reading continues and the"
                        + " response is returned unscanned, ruleId {}", limit, rule.getRuleId());
            }
            byte[] chunk = new byte[size];
            buffer.read(chunk);
            out.write(chunk, 0, size);
        } finally {
            DataBufferUtils.release(buffer);
        }
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
            return passThroughUnsupported(bytes, "the response carries Content-Encoding " + encoding);
        }
        if (!scannableContentType()) {
            return passThroughUnsupported(bytes, "the response has no scannable json content type");
        }
        JsonObject root;
        try {
            JsonElement element = JsonParser.parseString(new String(bytes, StandardCharsets.UTF_8));
            root = element.isJsonObject() ? element.getAsJsonObject() : null;
        } catch (RuntimeException e) {
            root = null;
        }
        if (Objects.isNull(root)) {
            return passThroughUnsupported(bytes, "the response body is not a JSON object");
        }
        JsonElement errorField = root.get("error");
        if (Objects.nonNull(errorField) && !errorField.isJsonNull() && errorField.isJsonObject()) {
            // a real upstream error envelope carries no model output, it is never
            // rewritten; a member that is json null is not an error, the responses
            // protocol carries "error": null on every successful completion
            return bytes;
        }
        PiiProtocolProfile profile = PiiProtocolProfiles.selectResponse(root);
        if (Objects.isNull(profile)) {
            return passThroughUnsupported(bytes, "no supported protocol profile matches the response");
        }
        List<PiiTextSlot> slots;
        PiiScanner.PiiOutcome outcome;
        try {
            slots = profile.responseSlots(root);
            outcome = new PiiScanner(rule, "response").scan(slots);
        } catch (PiiDetectorTimeoutException e) {
            return passThroughUnsupported(bytes, e.getMessage());
        }
        outcome.getAudits().forEach(auditSink::record);
        if (outcome.isBlocked()) {
            LOG.warn("pii detection: the response was rejected, ruleId {}", rule.getRuleId());
            return block();
        }
        return outcome.isChanged() ? root.toString().getBytes(StandardCharsets.UTF_8) : bytes;
    }

    /**
     * A response that this release cannot scan. The original bytes are handed
     * back untouched, they are never decoded and re encoded: a compressed body
     * for example does not survive a UTF-8 round trip.
     *
     * @param bytes the original payload bytes
     * @param reason why the payload cannot be scanned
     * @return the bytes to write, the original ones or the rejection body
     */
    private byte[] passThroughUnsupported(final byte[] bytes, final String reason) {
        if (rule.isFailClosed()) {
            LOG.warn("pii detection: {} and failClosed is set, the response is rejected", reason);
            return block();
        }
        LOG.warn("pii detection: {}, the response is passed through unscanned", reason);
        return bytes;
    }

    /**
     * A body that was aggregated past the bound of a fail open rule. The scan
     * is skipped, the complete body is written back unchanged.
     *
     * @param bytes the complete aggregated bytes
     * @return the same bytes
     */
    private byte[] passThroughUnscanned(final byte[] bytes) {
        return bytes;
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

    /**
     * Raised during a bounded aggregation of a fail closed rule: the reading of
     * the body is cancelled at the point the bound is exceeded.
     */
    private static final class ResponseTooLargeException extends RuntimeException {

        private static final long serialVersionUID = -1L;
    }
}
