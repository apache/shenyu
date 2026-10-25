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
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.pii.audit.PiiAuditSink;
import org.apache.shenyu.plugin.ai.pii.audit.LoggingPiiAuditSink;
import org.apache.shenyu.plugin.ai.pii.detector.PiiDetectorTimeoutException;
import org.apache.shenyu.plugin.ai.pii.handler.PiiDetectionPluginDataHandler;
import org.apache.shenyu.plugin.ai.pii.handler.PiiDetectionRule;
import org.apache.shenyu.plugin.ai.pii.profile.PiiProtocolProfile;
import org.apache.shenyu.plugin.ai.pii.profile.PiiProtocolProfiles;
import org.apache.shenyu.plugin.ai.pii.profile.PiiTextSlot;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.exception.ResponsiveException;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.ServerWebExchangeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

/**
 * The pii detection plugin. It detects personally identifiable information on
 * the message text fields of supported AI protocol payloads and applies the
 * action of the matched rule: block the direction, redact the matched ranges
 * inside the JSON tree, or audit the observation without the detected value.
 *
 * <p>The plugin runs at order 196, in front of the AI proxy that terminates
 * the chain. The first release covers non streaming payloads of the chat
 * completions and responses protocols: a streaming request under a response
 * scope is rejected up front when the rule is fail closed, otherwise the
 * response scan is skipped with a warning, and the response decorator passes
 * server sent events and compressed bodies through with a warning, or rejects
 * them when the rule is fail closed.</p>
 *
 * <p>Mutual exclusion: when the aiResponseTransformer plugin (order 66) is
 * enabled on the same traffic, its decorator sits inside this one and replaces
 * the whole response body with a generated one, so the redaction applied here
 * is overwritten. Do not enable both plugins on the same selector.</p>
 */
public class PiiDetectionPlugin extends AbstractShenyuPlugin {

    /**
     * The exchange attribute that marks a response whose scan was skipped on
     * the request side, the response decorator passes it through unscanned.
     */
    public static final String SKIP_RESPONSE_ATTRIBUTE = PiiDetectionPlugin.class.getName() + ".SKIP_RESPONSE";

    private static final Logger LOG = LoggerFactory.getLogger(PiiDetectionPlugin.class);

    private static final int BLOCK_CODE = HttpStatus.FORBIDDEN.value();

    private static final String BLOCK_MESSAGE = "Request rejected: sensitive content detected";

    private final List<HttpMessageReader<?>> readers;

    private final PiiAuditSink auditSink;

    public PiiDetectionPlugin(final List<HttpMessageReader<?>> readers) {
        this(readers, new LoggingPiiAuditSink());
    }

    public PiiDetectionPlugin(final List<HttpMessageReader<?>> readers, final PiiAuditSink auditSink) {
        this.readers = readers;
        this.auditSink = auditSink;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange,
                                   final ShenyuPluginChain chain,
                                   final SelectorData selector,
                                   final RuleData rule) {
        PiiDetectionRule piiRule = PiiDetectionPluginDataHandler.obtainRule(rule);
        if (Objects.isNull(piiRule)) {
            return chain.execute(exchange);
        }
        ServerWebExchange mutated = exchange;
        if (piiRule.scansResponse()) {
            mutated = exchange.mutate()
                    .response(new PiiResponseDecorator(exchange, piiRule, auditSink))
                    .build();
        }
        final ServerWebExchange effective = mutated;
        if (!piiRule.scansRequest()) {
            if (piiRule.scansResponse() && piiRule.isFailClosed()) {
                // a strict response scope still prechecks the enforcement mode of the
                // request: a streaming request under failClosed is rejected before it
                // reaches the model, even when the request text itself is not scanned
                return ServerWebExchangeUtils.rewriteRequestBody(effective, readers,
                                body -> precheckStreamingRequest(effective, body, piiRule))
                        .flatMap(chain::execute)
                        .onErrorResume(this::resume);
            }
            return chain.execute(effective).onErrorResume(this::resume);
        }
        long declaredSize = effective.getRequest().getHeaders().getContentLength();
        if (piiRule.isOversized(declaredSize)) {
            return oversized(exchange, chain, effective, piiRule, declaredSize);
        }
        return ServerWebExchangeUtils.rewriteRequestBody(effective, readers,
                        body -> inspectRequest(effective, body, piiRule))
                .flatMap(chain::execute)
                .onErrorResume(this::resume);
    }

    private Mono<Void> oversized(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                 final ServerWebExchange effective, final PiiDetectionRule rule, final long size) {
        if (rule.isFailClosed()) {
            LOG.warn("pii detection: the request of {} bytes exceeds maxBodySize {}, failClosed is set,"
                    + " the request is rejected", size, rule.getMaxBodySize());
            return reject(exchange);
        }
        LOG.warn("pii detection: the request of {} bytes exceeds maxBodySize {}, the body is not scanned",
                size, rule.getMaxBodySize());
        return chain.execute(effective);
    }

    private Mono<String> inspectRequest(final ServerWebExchange exchange,
                                        final String body, final PiiDetectionRule rule) {
        // the json parse, the regex scan and the audit logging never run on the netty
        // event loop, like the sensitive word plugin they hop to a bounded elastic worker
        return Mono.fromCallable(() -> inspectBody(exchange, body, rule))
                .subscribeOn(Schedulers.boundedElastic())
                .onErrorResume(PiiDetectorTimeoutException.class, timeout -> unsupportedMode(exchange, body, rule,
                        "the request scan exceeded its detector deadline"));
    }

    /**
     * The enforcement mode precheck of a strict response scope that does not scan
     * the request text. Only the streaming flag is looked at, a request that is
     * not streaming is forwarded unchanged, a streaming one is rejected before
     * the model is reached.
     */
    private Mono<String> precheckStreamingRequest(final ServerWebExchange exchange,
                                                  final String body, final PiiDetectionRule rule) {
        return Mono.fromCallable(() -> {
            JsonObject root = parse(body);
            if (Objects.nonNull(root) && isStreamingRequest(root)) {
                LOG.warn("pii detection: a streaming request under a strict response scope cannot enforce the"
                        + " response, the request is rejected before it reaches the model");
                throw rejectError(exchange);
            }
            return body;
        }).subscribeOn(Schedulers.boundedElastic());
    }

    private String inspectBody(final ServerWebExchange exchange,
                               final String body, final PiiDetectionRule rule) {
        // the bound is expressed in bytes, so the length is measured in utf 8
        // bytes and not in characters
        int byteLength = body.getBytes(StandardCharsets.UTF_8).length;
        if (rule.isOversized(byteLength)) {
            if (rule.isFailClosed()) {
                LOG.warn("pii detection: the body of {} bytes exceeds maxBodySize and failClosed is set,"
                        + " the request is rejected", byteLength);
                throw rejectError(exchange);
            }
            LOG.warn("pii detection: the body of {} bytes exceeds maxBodySize, the body is not scanned",
                    byteLength);
            return body;
        }
        JsonObject root = parse(body);
        if (Objects.isNull(root)) {
            return unsupportedBody(exchange, body, rule, "the request body is not a JSON object");
        }
        if (rule.scansResponse() && isStreamingRequest(root)) {
            if (rule.isFailClosed()) {
                LOG.warn("pii detection: a streaming request under a response scope cannot enforce the response,"
                        + " failClosed is set, the request is rejected");
                throw rejectError(exchange);
            }
            LOG.warn("pii detection: a streaming request under a response scope is not scanned on the response side");
            exchange.getAttributes().put(SKIP_RESPONSE_ATTRIBUTE, Boolean.TRUE);
        }
        PiiProtocolProfile profile = PiiProtocolProfiles.select(root);
        if (Objects.isNull(profile)) {
            return unsupportedBody(exchange, body, rule, "no supported protocol profile matches the payload");
        }
        List<PiiTextSlot> slots = profile.requestSlots(root);
        PiiScanner.PiiOutcome outcome = new PiiScanner(rule, "request").scan(slots);
        outcome.getAudits().forEach(auditSink::record);
        if (outcome.isBlocked()) {
            LOG.warn("pii detection: the request was rejected, ruleId {}", rule.getRuleId());
            throw rejectError(exchange);
        }
        return outcome.isChanged() ? root.toString() : body;
    }

    private String unsupportedBody(final ServerWebExchange exchange, final String body,
                                   final PiiDetectionRule rule, final String reason) {
        if (rule.isFailClosed()) {
            LOG.warn("pii detection: {} and failClosed is set, the request is rejected", reason);
            throw rejectError(exchange);
        }
        LOG.warn("pii detection: {}, the request is passed through unscanned", reason);
        return body;
    }

    private Mono<String> unsupportedMode(final ServerWebExchange exchange, final String body,
                                         final PiiDetectionRule rule, final String reason) {
        if (rule.isFailClosed()) {
            LOG.warn("pii detection: {} and failClosed is set, the request is rejected", reason);
            return Mono.error(rejectError(exchange));
        }
        LOG.warn("pii detection: {}, the request is passed through unscanned", reason);
        return Mono.just(body);
    }

    private Mono<Void> reject(final ServerWebExchange exchange) {
        return WebFluxResultUtils.failedResult(rejectError(exchange));
    }

    private ResponsiveException rejectError(final ServerWebExchange exchange) {
        return new ResponsiveException(BLOCK_CODE, BLOCK_MESSAGE, exchange);
    }

    private Mono<Void> resume(final Throwable error) {
        if (error instanceof ResponsiveException) {
            return WebFluxResultUtils.failedResult((ResponsiveException) error);
        }
        return Mono.error(error);
    }

    private JsonObject parse(final String body) {
        try {
            JsonElement element = JsonParser.parseString(body);
            return element.isJsonObject() ? element.getAsJsonObject() : null;
        } catch (RuntimeException e) {
            return null;
        }
    }

    private boolean isStreamingRequest(final JsonObject root) {
        JsonElement stream = root.get("stream");
        return Objects.nonNull(stream) && stream.isJsonPrimitive() && stream.getAsBoolean();
    }

    @Override
    public String named() {
        return PluginEnum.PII_DETECTION.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.PII_DETECTION.getCode();
    }
}
