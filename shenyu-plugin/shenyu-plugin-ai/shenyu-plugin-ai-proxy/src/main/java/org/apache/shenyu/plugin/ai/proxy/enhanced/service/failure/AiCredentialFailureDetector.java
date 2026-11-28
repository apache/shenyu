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

import java.net.URI;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;

/**
 * Detects declared credential failures while preserving single response-body ownership.
 */
public final class AiCredentialFailureDetector {

    /**
     * Maximum response prefix eligible for credential failure inspection.
     */
    public static final int MAX_INSPECTION_BYTES = AiProxyCredentialFailureInspector.MAX_INSPECTION_BYTES;

    private final AiProxyCredentialFailureClassifier classifier;

    /**
     * Create a detector from operator declarations and known protocols.
     *
     * @param properties credential failure declarations
     * @param protocols protocol registry
     */
    public AiCredentialFailureDetector(
            final AiProxyRetryProperties.CredentialFailureFallbackProperties properties,
            final AiProxyProtocolFactory protocols) {
        this.classifier = new AiProxyCredentialFailureClassifier(
                Objects.requireNonNull(properties, "properties must not be null"),
                Objects.requireNonNull(protocols, "protocols must not be null"));
    }

    /**
     * Whether a status can represent a credential failure.
     *
     * @param status HTTP status
     * @return true for credential-related statuses
     */
    public static boolean supportsStatus(final int status) {
        return AiProxyCredentialFailureClassifier.isCredentialStatus(status);
    }

    /**
     * Inspect a response only when an exact declaration applies.
     *
     * @param response upstream response
     * @param target credential failure detection target
     * @param consumer detection consumer retaining response body ownership
     * @param <T> consumer result type
     * @return consumer result
     */
    public <T> Mono<T> inspect(final AiUpstreamResponse response, final DetectionTarget target,
                               final Function<Detection, Mono<T>> consumer) {
        List<AiProxyCredentialFailureClassifier.Rule> rules =
                classifier.findRules(target.endpoint(), target.protocol(), response);
        if (rules.isEmpty()) {
            return consumer.apply(new Detection(response, false));
        }
        return AiProxyCredentialFailureInspector.inspect(response,
                body -> classifier.matches(body, rules),
                inspection -> consumer.apply(
                        new Detection(inspection.response(), inspection.credentialFailure())));
    }

    /**
     * Credential failure detection result with the original response body preserved.
     *
     * @param response preserved upstream response
     * @param credentialFailure whether a declared credential failure matched
     */
    public record Detection(AiUpstreamResponse response, boolean credentialFailure) {
    }

    /**
     * Exact upstream endpoint and target protocol used for declared failure matching.
     *
     * @param endpoint exact upstream endpoint
     * @param protocol target protocol
     */
    public record DetectionTarget(URI endpoint, String protocol) {

        public DetectionTarget {
            Objects.requireNonNull(endpoint, "endpoint must not be null");
            Objects.requireNonNull(protocol, "protocol must not be null");
        }
    }
}
