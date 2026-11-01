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

package org.apache.shenyu.plugin.api.utils;

import org.apache.shenyu.common.utils.ObjectTypeUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.failure.GatewayFailure;
import org.apache.shenyu.plugin.api.failure.GatewayFailureMapping;
import org.apache.shenyu.plugin.api.failure.GatewayFailurePresentation;
import org.apache.shenyu.plugin.api.exception.ResponsiveException;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.http.HttpStatusCode;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.Optional;

/**
 * The type Shenyu result utils.
 */
public final class WebFluxResultUtils {

    public static final String OUTPUT_STARTED_ATTRIBUTE = WebFluxResultUtils.class.getName() + ".outputStarted";

    /**
     * result utils log.
     */
    private static final Logger LOG = LoggerFactory.getLogger(WebFluxResultUtils.class);

    private WebFluxResultUtils() {
    }

    /**
     * Response result.
     * An existing response content type takes precedence over the result's default.
     *
     * @param exchange the exchange
     * @param result   the result
     * @return the result
     */
    public static Mono<Void> result(final ServerWebExchange exchange, final Object result) {
        if (Objects.isNull(result)) {
            return Mono.empty();
        }
        return Mono.defer(() -> {
            if (exchange.getResponse().isCommitted() || !claim(exchange)) {
                return Mono.error(new IllegalStateException("Response output already started"));
            }
            return writeResult(exchange, result);
        });
    }

    private static boolean claim(final ServerWebExchange exchange) {
        return Objects.isNull(exchange.getAttributes().putIfAbsent(OUTPUT_STARTED_ATTRIBUTE, Boolean.TRUE));
    }

    private static Mono<Void> writeResult(final ServerWebExchange exchange, final Object result) {
        if (Objects.isNull(result)) {
            return Mono.empty();
        }
        final ShenyuResult<?> shenyuResult = ShenyuResultWrap.shenyuResult();
        Object resultData = shenyuResult.format(exchange, result);
        if (Objects.isNull(exchange.getResponse().getHeaders().getContentType())) {
            // basic data use text/plain
            MediaType mediaType = MediaType.TEXT_PLAIN;
            if (!ObjectTypeUtils.isBasicType(result)) {
                mediaType = shenyuResult.contentType(exchange, resultData);
            }
            exchange.getResponse().getHeaders().setContentType(mediaType);
        }
        final Object responseData = shenyuResult.result(exchange, resultData);
        Objects.requireNonNull(responseData);
        final byte[] bytes = (responseData instanceof byte[])
                ? (byte[]) responseData : responseData.toString().getBytes(StandardCharsets.UTF_8);
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse()
                .bufferFactory().wrap(bytes))
                .doOnNext(data -> exchange.getResponse().getHeaders().setContentLength(data.readableByteCount())));
    }

    /**
     * get no selector result.
     *
     * @param pluginName the plugin name
     * @param exchange   the exchange
     * @return the mono
     */
    public static Mono<Void> noSelectorResult(final String pluginName, final ServerWebExchange exchange) {
        LOG.error("can not match selector data: {} , path is {}", pluginName, exchange.getRequest().getURI().getRawPath());
        return failureResult(exchange, failure(exchange, GatewayFailure.Reason.SELECTOR_MISS, GatewayFailure.Stage.SELECTOR_MATCH, pluginName),
                null, ShenyuResultEnum.SELECTOR_NOT_FOUND.getCode(), pluginName + ":" + ShenyuResultEnum.SELECTOR_NOT_FOUND.getMsg(), null);
    }

    /**
     * get no rule result.
     *
     * @param pluginName the plugin name
     * @param exchange   the exchange
     * @return the mono
     */
    public static Mono<Void> noRuleResult(final String pluginName, final ServerWebExchange exchange) {
        LOG.error("can not match rule data: {} , path is {}", pluginName, exchange.getRequest().getURI().getRawPath());
        return failureResult(exchange, failure(exchange, GatewayFailure.Reason.RULE_MISS, GatewayFailure.Stage.RULE_MATCH, pluginName),
                null, ShenyuResultEnum.RULE_NOT_FOUND.getCode(), pluginName + ":" + ShenyuResultEnum.RULE_NOT_FOUND.getMsg(), null);
    }

    /**
     * Construct HTTP-facing facts from an adopted local owner, never from request headers or payloads.
     * @param exchange current response ownership
     * @param reason explicit reason
     * @param stage adopted owner
     * @param plugin local plugin identity
     * @return immutable classification
     */
    public static GatewayFailure failure(final ServerWebExchange exchange, final GatewayFailure.Reason reason,
                                         final GatewayFailure.Stage stage, final String plugin) {
        ShenyuContext context = exchange.getAttribute(Constants.CONTEXT);
        GatewayFailure.Protocol protocol = Objects.isNull(context) ? GatewayFailure.Protocol.UNKNOWN
                : "http".equals(context.getRpcType()) ? GatewayFailure.Protocol.HTTP : GatewayFailure.Protocol.HTTP_BRIDGE;
        boolean started = exchange.getResponse().isCommitted() || exchange.getAttributes().containsKey(OUTPUT_STARTED_ATTRIBUTE);
        return new GatewayFailure(reason, stage, protocol, started ? GatewayFailure.Output.STARTED_OR_COMMITTED : GatewayFailure.Output.BEFORE_ERROR_WRITE, plugin);
    }

    /**
     * Claim the existing result writer once before applying a presentation. Internal causes never enter mapper facts.
     * @param exchange error response
     * @param facts body-free classification
     * @param legacyStatus old status assignment, null preserves the historical unset status
     * @param legacyCode old result code
     * @param legacyMessage old public message, retained only for compatibility
     * @param cause internal propagation when output already started, nullable for a direct rejection
     * @return the existing error write, or original error after commitment/cancellation
     */
    public static Mono<Void> failureResult(final ServerWebExchange exchange, final GatewayFailure facts, final HttpStatusCode legacyStatus,
                                           final int legacyCode, final String legacyMessage, final Throwable cause) {
        return Mono.defer(() -> {
            exchange.getAttributes().put(GatewayFailureMapping.FACTS_ATTRIBUTE, facts);
            if (facts.reason() == GatewayFailure.Reason.CANCELLATION || exchange.getResponse().isCommitted() || !claim(exchange)) {
                return Objects.isNull(cause) ? Mono.empty() : Mono.error(cause);
            }
            Optional<GatewayFailurePresentation> selected = GatewayFailureMapping.current().select(facts);
            if (selected.isPresent()) {
                var presentation = selected.get();
                exchange.getResponse().setStatusCode(HttpStatusCode.valueOf(presentation.status()));
                return presentation.emptyBody() ? exchange.getResponse().setComplete()
                        : writeResult(exchange, ShenyuResultWrap.error(exchange, presentation.code(), presentation.message(), null));
            }
            if (Objects.nonNull(legacyStatus)) {
                exchange.getResponse().setStatusCode(legacyStatus);
            }
            return writeResult(exchange, ShenyuResultWrap.error(exchange, legacyCode, legacyMessage, null));
        });
    }

    /**
     * get failed result.
     *
     * @param responsiveException responsiveException
     * @return the mono.
     */
    public static Mono<Void> failedResult(final ResponsiveException responsiveException) {
        return failedResult(responsiveException.getCode(),
                responsiveException.getMessage(),
                responsiveException.getWebExchange());
    }

    /**
     * get failed result.
     *
     * @param code     code
     * @param reason   reason
     * @param exchange exchange
     * @return the mono.
     */
    public static Mono<Void> failedResult(final int code, final String reason, final ServerWebExchange exchange) {
        Object error = ShenyuResultWrap.error(exchange, code, reason, null);
        return WebFluxResultUtils.result(exchange, error);
    }

}
