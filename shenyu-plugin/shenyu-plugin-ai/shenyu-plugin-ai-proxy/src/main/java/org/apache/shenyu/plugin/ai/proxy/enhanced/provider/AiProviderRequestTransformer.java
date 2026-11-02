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

package org.apache.shenyu.plugin.ai.proxy.enhanced.provider;

import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.Objects;

/**
 * Transforms a standard target-protocol payload into a provider dialect.
 */
@FunctionalInterface
public interface AiProviderRequestTransformer {

    /**
     * Transform a provider-bound request body.
     *
     * @param payload target-protocol payload
     * @param context provider and request context
     * @return provider-bound payload
     */
    ObjectNode transform(ObjectNode payload, AiProviderRequestContext context);

    /**
     * Compose this transformer with another transformer.
     *
     * @param next transformer applied to this transformer's result
     * @return composed transformer
     */
    default AiProviderRequestTransformer andThen(final AiProviderRequestTransformer next) {
        Objects.requireNonNull(next, "next AI provider request transformer is required");
        return (payload, context) -> next.transform(transform(payload, context), context);
    }

    /**
     * Return the payload unchanged.
     *
     * @return identity transformer
     */
    static AiProviderRequestTransformer identity() {
        return (payload, context) -> payload;
    }
}
