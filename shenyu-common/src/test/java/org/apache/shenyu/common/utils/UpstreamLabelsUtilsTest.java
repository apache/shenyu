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

package org.apache.shenyu.common.utils;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests strict label types and immutable snapshots shared by Admin and gateways.
 */
class UpstreamLabelsUtilsTest {

    @Test
    void testOmittedAndEmptyLabelsClearExistingValues() {
        assertTrue(UpstreamLabelsUtils.parse(null).isEmpty());
        assertTrue(UpstreamLabelsUtils.parse(new JsonObject()).isEmpty());
    }

    @Test
    void testParsingPreservesStringsAndReturnsIndependentSnapshot() {
        JsonObject input = new JsonObject();
        input.addProperty("release", "stable");
        input.addProperty("region", "上海");
        Map<String, String> labels = UpstreamLabelsUtils.parse(input);
        input.addProperty("release", "next");
        assertEquals(Map.of("release", "stable", "region", "上海"), labels);
        assertThrows(UnsupportedOperationException.class, () -> labels.put("release", "next"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"null", "[]", "1", "true", "\"stable\"", "{\"release\":1}", "{\"release\":true}",
        "{\"release\":null}", "{\"release\":{}}", "{\"release\":[]}", "{\"\":\"stable\"}",
        "{\" \":\"stable\"}", "{\"release\":\"\"}", "{\"release\":\" \"}"})
    void testInvalidLabelsAreRejectedWithoutCoercion(final String input) {
        JsonElement labels = GsonUtils.getInstance().fromJson(input, JsonElement.class);
        assertThrows(IllegalArgumentException.class, () -> UpstreamLabelsUtils.parse(labels));
    }
}
