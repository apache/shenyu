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

package org.apache.shenyu.plugin.base.utils;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link UpstreamProps}.
 */
class UpstreamPropsTest {

    @Test
    void testParseNullPropsAppliesDefaults() {
        final UpstreamProps props = UpstreamProps.parse(null);

        assertEquals(10, props.getWarmup());
        assertFalse(props.isGray());
        assertTrue(props.isHealthCheckEnabled());
        assertTrue(props.toMap().isEmpty());
    }

    @Test
    void testParsePropsOverridesDefaults() {
        final UpstreamProps props = UpstreamProps.parse("{\"warmup\":\"20\",\"gray\":\"true\",\"healthCheckEnabled\":\"false\"}");

        assertEquals(20, props.getWarmup());
        assertTrue(props.isGray());
        assertFalse(props.isHealthCheckEnabled());
    }

    @Test
    void testDrainingDefaultsToFalseAndParsesBothSpellings() {
        assertFalse(UpstreamProps.parse(null).isDraining());
        assertFalse(UpstreamProps.parse("{\"warmup\":\"20\"}").isDraining());
        assertTrue(UpstreamProps.parse("{\"draining\":\"true\"}").isDraining());
        assertTrue(UpstreamProps.parse("{\"draining\":true}").isDraining());
        assertFalse(UpstreamProps.parse("{\"draining\":\"false\"}").isDraining());
    }

    @Test
    void testReadyDefaultsToTrueAndParsesBothSpellings() {
        assertTrue(UpstreamProps.parse(null).isReady());
        assertTrue(UpstreamProps.parse("{\"warmup\":\"20\"}").isReady());
        assertFalse(UpstreamProps.parse("{\"notReady\":\"true\"}").isReady());
        assertFalse(UpstreamProps.parse("{\"notReady\":true}").isReady());
        assertTrue(UpstreamProps.parse("{\"notReady\":\"false\"}").isReady());
    }

    @Test
    void testToMapReturnsCopyOfAllProps() {
        final UpstreamProps props = UpstreamProps.parse("{\"warmup\":\"20\",\"az\":\"az1\"}");
        final Map<String, String> map = props.toMap();

        assertEquals("20", map.get("warmup"));
        assertEquals("az1", map.get("az"));

        map.put("az", "az2");
        assertEquals("az1", props.toMap().get("az"));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "{\"warmup\":\"20\",\"gray\":\"true\",\"healthCheckEnabled\":\"false\",\"labels\":{\"release\":\"stable\"}}",
        "{\"warmup\":20,\"gray\":true,\"healthCheckEnabled\":false,\"labels\":{\"release\":\"stable\"}}"
    })
    void testLabelsRemainSeparateFromScalarProperties(final String input) {
        UpstreamProps props = UpstreamProps.parse(input);
        assertEquals(20, props.getWarmup());
        assertTrue(props.isGray());
        assertFalse(props.isHealthCheckEnabled());
        assertEquals(Map.of("release", "stable"), props.getLabels());
        assertEquals(Map.of("warmup", "20", "gray", "true", "healthCheckEnabled", "false"), props.toMap());
        assertThrows(UnsupportedOperationException.class, () -> props.getLabels().put("release", "next"));
    }

    @Test
    void testNestedExtensionPropertiesDoNotBreakScalarParsing() {
        UpstreamProps props = UpstreamProps.parse("{\"az\":\"az1\",\"custom\":{\"tls\":true},\"items\":[1],\"optional\":null}");
        assertEquals("az1", props.toMap().get("az"));
        assertNull(props.toMap().get("optional"));
        assertTrue(props.toMap().containsKey("optional"));
        assertFalse(props.toMap().containsKey("custom"));
        assertFalse(props.toMap().containsKey("items"));
        assertTrue(props.getLabels().isEmpty());
    }

    @ParameterizedTest
    @ValueSource(strings = {"{}", "null", "{\"labels\":{}}", "{\"healthCheckEnabled\":null}"})
    void testEmptyPropertiesRetainDefaults(final String input) {
        UpstreamProps props = UpstreamProps.parse(input);
        assertEquals(10, props.getWarmup());
        assertFalse(props.isGray());
        assertTrue(props.isHealthCheckEnabled());
        assertTrue(props.getLabels().isEmpty());
    }
}
