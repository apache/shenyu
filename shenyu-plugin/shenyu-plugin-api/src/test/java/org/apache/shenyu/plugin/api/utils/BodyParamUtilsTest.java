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

import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

/**
 * Test case for {@link BodyParamUtils}.
 */
public final class BodyParamUtilsTest {

    @Test
    public void testSingleNameMappedParameterUsesResolvedTypeAndValue() {
        Pair<String[], Object[]> result = BodyParamUtils.buildParameters(
                "{\"arg\":{\"name\":\"Ada\"}}", "{\"arg\":\"java.util.Map\"}");
        assertArrayEquals(new String[]{"java.util.Map"}, result.getLeft());
        assertEquals(1, result.getRight().length);
        Map<?, ?> argument = assertInstanceOf(Map.class, result.getRight()[0]);
        assertEquals("Ada", argument.get("name"));
        assertFalse(argument.containsKey("arg"), "argument must not keep the name-mapping wrapper");
    }

    @Test
    public void testMultiNameMappedParametersKeepNameOrder() {
        Pair<String[], Object[]> result = BodyParamUtils.buildParameters(
                "{\"first\":\"one\",\"second\":\"two\"}",
                "{\"first\":\"java.lang.String\",\"second\":\"java.lang.String\"}");
        assertArrayEquals(new String[]{"java.lang.String", "java.lang.String"}, result.getLeft());
        assertArrayEquals(new Object[]{"one", "two"}, result.getRight());
    }

    @Test
    public void testPlainSingleBeanKeepsWholeBodySemantics() {
        Pair<String[], Object[]> result = BodyParamUtils.buildParameters(
                "{\"name\":\"Ada\"}", "com.example.User");
        assertArrayEquals(new String[]{"com.example.User"}, result.getLeft());
        Map<?, ?> argument = assertInstanceOf(Map.class, result.getRight()[0]);
        assertEquals("Ada", argument.get("name"));
    }

    @Test
    public void testSingleNameMappedBaseTypeIsUnchanged() {
        Pair<String[], Object[]> result = BodyParamUtils.buildParameters(
                "{\"arg\":\"value\"}", "{\"arg\":\"java.lang.String\"}");
        assertArrayEquals(new String[]{"java.lang.String"}, result.getLeft());
        assertArrayEquals(new Object[]{"value"}, result.getRight());
    }
}
