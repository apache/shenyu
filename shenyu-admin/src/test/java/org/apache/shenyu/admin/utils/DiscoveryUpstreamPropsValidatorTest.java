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

package org.apache.shenyu.admin.utils;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Tests submitted property validation, including omitted and explicitly empty labels.
 */
class DiscoveryUpstreamPropsValidatorTest {

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"{}", "{\"labels\":{}}", "{\"labels\":{\"release\":\"canary\"}}", "{\"warmup\":20}",
        "{\"extension\":{\"nested\":[1,true,null]}}"})
    void testValidProps(final String props) {
        assertDoesNotThrow(() -> DiscoveryUpstreamPropsValidator.validate(props));
    }

    @ParameterizedTest
    @ValueSource(strings = {"null", "[]", "broken", "{\"labels\":null}", "{\"labels\":[]}",
        "{\"labels\":{\"release\":1}}", "{\"labels\":{\"release\":true}}", "{\"labels\":{\"release\":null}}",
        "{\"labels\":{\" \" :\"stable\"}}"})
    void testInvalidPropsAreRejected(final String props) {
        assertThrows(ShenyuAdminException.class, () -> DiscoveryUpstreamPropsValidator.validate(props));
    }
}
