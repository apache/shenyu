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

package org.apache.shenyu.plugin.mock.api;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Tests for {@link MockRequest}.
 */
public final class MockRequestTest {

    @Test
    public void testGetFormsDecodesUtf8FormBody() {
        MockRequest request = request("name=shenyu&message=hello+world&city=%E5%8C%97%E4%BA%AC&plus=%2B");

        Map<String, String> forms = request.getForms();

        assertEquals("shenyu", forms.get("name"));
        assertEquals("hello world", forms.get("message"));
        assertEquals("北京", forms.get("city"));
        assertEquals("+", forms.get("plus"));
    }

    @Test
    public void testGetFormsUsesFirstValueForDuplicateKeys() {
        MockRequest request = request("name=first&name=second");

        assertEquals("first", request.getForms().get("name"));
    }

    @Test
    public void testGetFormsReturnsEmptyMapForEmptyOrMissingBody() {
        MockRequest emptyBodyRequest = request("");
        MockRequest missingBodyRequest = MockRequest.Builder.builder().build();

        assertEquals(Map.of(), emptyBodyRequest.getForms());
        assertEquals(Map.of(), missingBodyRequest.getForms());
    }

    @Test
    public void testGetFormsCachesParsedMap() {
        MockRequest request = request("name=shenyu");

        assertSame(request.getForms(), request.getForms());
    }

    @Test
    public void testGetFormsRejectsMalformedPercentEncoding() {
        MockRequest request = request("name=%XX");

        assertThrows(IllegalArgumentException.class, request::getForms);
    }

    private MockRequest request(final String body) {
        return MockRequest.Builder.builder().body(body.getBytes(StandardCharsets.UTF_8)).build();
    }
}
