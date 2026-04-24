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

package org.apache.shenyu.admin.service.converter;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * Test case for {@link SelectorHandleConverterFactor}.
 */
class SelectorHandleConverterFactorTest {

    private SelectorHandleConverterFactor factor;

    private DivideSelectorHandleConverter divideConverter;

    private DubboSelectorHandleConverter dubboConverter;

    private GrpcSelectorHandleConverter grpcConverter;

    @BeforeEach
    void setUp() {
        divideConverter = new DivideSelectorHandleConverter();
        dubboConverter = new DubboSelectorHandleConverter();
        grpcConverter = new GrpcSelectorHandleConverter();

        Map<String, SelectorHandleConverter> maps = new HashMap<>();
        maps.put("divide", divideConverter);
        maps.put("dubbo", dubboConverter);
        maps.put("grpc", grpcConverter);

        factor = new SelectorHandleConverterFactor(maps);
    }

    @ParameterizedTest
    @CsvSource({
        "divide,divide",
        "dubbo,dubbo",
        "grpc,grpc"
    })
    void testNewInstanceWithValidPlugins(final String pluginName, final String expectedPluginName) {
        SelectorHandleConverter converter = factor.newInstance(pluginName);
        assertNotNull(converter);
        assertEquals(expectedPluginName, converter.pluginName());

        if ("divide".equals(pluginName)) {
            assertSame(divideConverter, converter);
        } else if ("dubbo".equals(pluginName)) {
            assertSame(dubboConverter, converter);
        } else if ("grpc".equals(pluginName)) {
            assertSame(grpcConverter, converter);
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"nonexistent", "unknown", "invalid"})
    void testNewInstanceWithInvalidPlugins(final String pluginName) {
        SelectorHandleConverter converter = factor.newInstance(pluginName);
        assertNull(converter);
    }

    @Test
    void testFactorWithEmptyMap() {
        SelectorHandleConverterFactor emptyFactor = new SelectorHandleConverterFactor(new HashMap<>());
        SelectorHandleConverter converter = emptyFactor.newInstance("divide");
        assertNull(converter);
    }
}
