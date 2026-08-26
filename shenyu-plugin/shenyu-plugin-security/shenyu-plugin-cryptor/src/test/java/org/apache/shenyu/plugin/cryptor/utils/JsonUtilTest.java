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

package org.apache.shenyu.plugin.cryptor.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class JsonUtilTest {

    @Test
    public void testParser() {
        assertEquals("shenyu", JsonUtil.parser("{\"name\":\"shenyu\"}", "name"));
        assertEquals("shenyu", JsonUtil.parser("{\"data\":{\"nested\":{\"name\":\"shenyu\"}}}", "data.nested.name"));
    }

    @Test
    public void testParserReturnsNullForNonObjectBody() {
        assertNull(JsonUtil.parser("[{\"name\":\"shenyu\"}]", "name"));
        assertNull(JsonUtil.parser("\"shenyu\"", "name"));
        assertNull(JsonUtil.parser("invalid", "name"));
    }

    @Test
    public void testParserReturnsNullForInvalidNestedPath() {
        assertNull(JsonUtil.parser("{}", "data.nested.name"));
        assertNull(JsonUtil.parser("{\"data\":\"shenyu\"}", "data.nested.name"));
        assertNull(JsonUtil.parser("{\"data\":{}}", "data.nested.name"));
        assertNull(JsonUtil.parser("{\"data\":{\"nested\":[]}}", "data.nested.name"));
        assertNull(JsonUtil.parser("{\"data\":{\"nested\":{}}}", "data.nested.name"));
        assertNull(JsonUtil.parser("{\"data\":{\"nested\":{\"name\":{}}}}", "data.nested.name"));
    }
}
