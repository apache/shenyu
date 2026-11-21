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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.Iterator;
import java.util.Map;
import java.util.Objects;

/**
 * Deep-merges configured request-body fields into a provider-bound payload.
 */
final class AiRequestBodyMerger {

    private static final int MAX_MERGE_DEPTH = 16;

    private AiRequestBodyMerger() {
    }

    /**
     * Recursively merge object fields. Arrays, scalars and type mismatches are replaced as a whole.
     *
     * @param target outgoing request body
     * @param patch configured object patch
     * @param force whether configured values overwrite existing values
     * @return merged target
     */
    static ObjectNode merge(final ObjectNode target, final ObjectNode patch, final boolean force) {
        validateDepth(patch, 0);
        return mergeObjects(target, patch, force);
    }

    private static ObjectNode mergeObjects(final ObjectNode target, final ObjectNode patch, final boolean force) {
        Iterator<Map.Entry<String, JsonNode>> fields = patch.fields();
        while (fields.hasNext()) {
            Map.Entry<String, JsonNode> field = fields.next();
            JsonNode current = target.get(field.getKey());
            JsonNode value = field.getValue();
            if (value.isObject() && Objects.nonNull(current) && current.isObject()) {
                mergeObjects((ObjectNode) current, (ObjectNode) value, force);
            } else if (Objects.isNull(current) || force) {
                target.set(field.getKey(), value.deepCopy());
            }
        }
        return target;
    }

    private static void validateDepth(final JsonNode node, final int depth) {
        if (depth > MAX_MERGE_DEPTH) {
            throw new IllegalArgumentException("AI request-body override exceeds maximum depth " + MAX_MERGE_DEPTH);
        }
        if (node.isContainerNode()) {
            node.elements().forEachRemaining(child -> validateDepth(child, depth + 1));
        }
    }
}
