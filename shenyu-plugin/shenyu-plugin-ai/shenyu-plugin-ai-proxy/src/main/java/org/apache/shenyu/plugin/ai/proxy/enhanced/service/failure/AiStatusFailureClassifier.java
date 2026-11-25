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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service.failure;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyRetryProperties;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

/**
 * Classifies upstream HTTP failures without choosing an orchestration transition.
 */
public final class AiStatusFailureClassifier {

    private final AiCredentialFailureDetector credentialFailureDetector;

    public AiStatusFailureClassifier(
            final AiProxyRetryProperties.CredentialFailureFallbackProperties credentialProperties,
            final AiProxyProtocolFactory protocols) {
        this.credentialFailureDetector = new AiCredentialFailureDetector(
                Objects.requireNonNull(credentialProperties, "credentialProperties must not be null"),
                Objects.requireNonNull(protocols, "protocols must not be null"));
    }

    /**
     * Classify an HTTP failure, inspecting its bounded body only when an exact target is supplied.
     *
     * @param response upstream failure response
     * @param credentialTarget exact target eligible for credential failure detection
     * @param consumer classified failure consumer retaining response-body ownership
     * @param <T> consumer result type
     * @return consumer result
     */
    public <T> Mono<T> classify(
            final AiUpstreamResponse response,
            final Optional<AiCredentialFailureDetector.DetectionTarget> credentialTarget,
            final Function<AiStatusFailure, Mono<T>> consumer) {
        Objects.requireNonNull(response, "response must not be null");
        Objects.requireNonNull(credentialTarget, "credentialTarget must not be null");
        Objects.requireNonNull(consumer, "consumer must not be null");
        return Mono.defer(() -> credentialTarget
                .map(target -> credentialFailureDetector.inspect(response, target,
                        detection -> consumer.apply(new AiStatusFailure(
                                detection.response(), detection.credentialFailure()))))
                .orElseGet(() -> consumer.apply(new AiStatusFailure(response, false))));
    }
}
