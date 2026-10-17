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

import com.google.gson.JsonParser;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests the instance information contract and its bounded legacy compatibility.
 */
class InstanceInfoUtilsTest {

    @Test
    void testCurrentAndLegacyInformationHaveTheSameRepresentation() {
        String information = "{\"cpu\":4,\"os\":\"Windows\"}";
        String legacy = GsonUtils.getInstance().toJson(information);

        assertEquals(information, InstanceInfoUtils.normalize(information));
        assertEquals(information, InstanceInfoUtils.normalize(legacy));
        assertEquals(information, InstanceInfoUtils.normalize(InstanceInfoUtils.normalize(legacy)));
    }

    @Test
    void testNestedStringsAreNotDecoded() {
        String information = "{\"note\":\"{\\\"nested\\\":true}\",\"path\":\"C:\\\\test\",\"label\":\"机器\"}";

        assertEquals(JsonParser.parseString(information), JsonParser.parseString(InstanceInfoUtils.normalize(information)));
    }

    @Test
    void testOnlyOneLegacyWrapperIsDecoded() {
        String information = "{\"cpu\":4}";
        String overEncoded = GsonUtils.getInstance().toJson(GsonUtils.getInstance().toJson(information));

        assertEquals("{}", InstanceInfoUtils.normalize(overEncoded));
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" ", "null", "[]", "true", "42", "not-json", "{broken}", "\"plain text\""})
    void testInvalidOptionalInformationBecomesAnEmptyObject(final String information) {
        assertEquals("{}", InstanceInfoUtils.normalize(information));
    }
}
