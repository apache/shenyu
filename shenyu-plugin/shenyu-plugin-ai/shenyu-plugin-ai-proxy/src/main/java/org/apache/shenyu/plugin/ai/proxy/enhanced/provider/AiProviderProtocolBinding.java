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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.net.URI;
import java.util.Map;
import java.util.Objects;

/**
 * Provider behavior bound to one native target protocol.
 *
 * @param path relative upstream operation path
 * @param requestTransformer provider request transformer
 */
public record AiProviderProtocolBinding(String path, AiProviderRequestTransformer requestTransformer) {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    /**
     * Validate a protocol binding.
     */
    public AiProviderProtocolBinding {
        URI relative = URI.create(path);
        if (path.isBlank() || relative.isAbsolute() || path.startsWith("//") || Objects.nonNull(relative.getRawQuery())
                || Objects.nonNull(relative.getRawFragment())) {
            throw new IllegalArgumentException("AI provider path must be a relative path without query or fragment");
        }
        Objects.requireNonNull(requestTransformer, "AI provider request transformer is required");
    }

    /**
     * Apply provider behavior and then the configured patch for this target protocol.
     *
     * @param payload target-protocol payload
     * @param context provider and request context
     * @return provider-bound payload
     */
    public ObjectNode transform(final ObjectNode payload, final AiProviderRequestContext context) {
        ObjectNode transformed = requestTransformer.transform(payload, context);
        Map<String, Map<String, Object>> overrides = context.config().getRequestBodyOverrides();
        if (Objects.isNull(overrides)) {
            return transformed;
        }
        Map<String, Object> patch = overrides.get(context.targetProtocol());
        if (Objects.isNull(patch)) {
            return transformed;
        }
        return AiRequestBodyMerger.merge(transformed, OBJECT_MAPPER.valueToTree(patch),
                Boolean.TRUE.equals(context.config().getRequestBodyForceOverride()));
    }
}
