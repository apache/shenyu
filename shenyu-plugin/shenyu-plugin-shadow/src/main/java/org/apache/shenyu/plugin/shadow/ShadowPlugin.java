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

package org.apache.shenyu.plugin.shadow;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.shadow.capture.ShadowBodyCapture;
import org.apache.shenyu.plugin.shadow.capture.ShadowRequestTee;
import org.apache.shenyu.plugin.shadow.config.ShadowEligibility;
import org.apache.shenyu.plugin.shadow.config.ShadowSampler;
import org.apache.shenyu.plugin.shadow.config.ShadowSkipReason;
import org.apache.shenyu.plugin.shadow.dispatch.ShadowRuntime;
import org.apache.shenyu.plugin.shadow.dispatch.ShadowRuntimeRegistry;
import org.apache.shenyu.plugin.shadow.dispatch.ShadowTask;
import org.apache.shenyu.plugin.shadow.dispatch.ShadowTaskFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.util.Objects;
import java.util.Optional;

/**
 * Shadow traffic plugin.
 *
 * <p>The plugin mirrors a bounded, sampled copy of an eligible HTTP request to
 * an independent shadow destination. The primary request never waits for the
 * shadow call: dispatch happens only after the primary exchange terminates, and
 * every resource bound is expressed in the rule handle.</p>
 */
public class ShadowPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(ShadowPlugin.class);

    private final ShadowRuntimeRegistry runtimeRegistry;

    public ShadowPlugin(final ShadowRuntimeRegistry runtimeRegistry) {
        this.runtimeRegistry = Objects.requireNonNull(runtimeRegistry);
    }

    @Override
    public int getOrder() {
        return PluginEnum.SHADOW.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.SHADOW.getName();
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        ShadowRuntime runtime = runtimeRegistry.find(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(runtime)) {
            return chain.execute(exchange);
        }
        Optional<ShadowSkipReason> skipped = ShadowEligibility.check(exchange, runtime.getHandle());
        if (skipped.isPresent()) {
            runtime.recordSkip(skipped.get());
            return chain.execute(exchange);
        }
        if (!ShadowSampler.sample(exchange, rule, runtime.getHandle().getSampleRate())) {
            return chain.execute(exchange);
        }
        ShadowBodyCapture capture = new ShadowBodyCapture(runtime.getHandle().getMaxRequestBodyBytes());
        ServerWebExchange observed = ShadowRequestTee.tee(exchange, capture);
        return chain.execute(observed).doFinally(signal -> dispatch(runtime, observed, capture, signal));
    }

    /**
     * Dispatch the mirrored request after the primary exchange terminated.
     *
     * <p>A cancelled primary says nothing about the shadow target, and a body
     * that exceeded its bound must not be sent partially, so both cases are
     * counted as skips instead of producing a misleading comparison.</p>
     *
     * @param runtime the rule runtime
     * @param exchange the observed exchange
     * @param capture the bounded body capture
     * @param signal the primary terminal signal
     */
    private void dispatch(final ShadowRuntime runtime, final ServerWebExchange exchange,
                          final ShadowBodyCapture capture, final SignalType signal) {
        if (SignalType.CANCEL.equals(signal)) {
            runtime.recordSkip(ShadowSkipReason.PRIMARY_CANCELLED);
            return;
        }
        if (capture.isTruncated()) {
            runtime.recordSkip(ShadowSkipReason.BODY_TOO_LARGE);
            return;
        }
        byte[] body = capture.bytes();
        long declaredLength = exchange.getRequest().getHeaders().getContentLength();
        if (declaredLength >= 0 && body.length != declaredLength) {
            // The primary did not read the complete declared body, so a copy would
            // carry a different payload than the request it claims to mirror.
            runtime.recordSkip(ShadowSkipReason.BODY_INCOMPLETE);
            return;
        }
        Optional<ShadowTask> task = ShadowTaskFactory.create(exchange, runtime.getHandle(), body);
        if (task.isEmpty()) {
            runtime.recordSkip(ShadowSkipReason.INVALID_TARGET);
            return;
        }
        if (!runtime.mirror(task.get())) {
            runtime.recordSkip(ShadowSkipReason.QUEUE_OVERFLOW);
            LOG.debug("shadow queue is full, sample dropped");
        }
    }
}
