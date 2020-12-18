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

package org.dromara.soul.common.utils;

import org.hamcrest.collection.IsMapContaining;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ErrorCollector;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import java.util.Map;

import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

/**
 * Test cases for HttpParamConverter.
 *
 * @author dengliming
 */
public final class HttpParamConverterTest {

    @Rule
    public ErrorCollector collector = new ErrorCollector();

    @Test
    public void testOfString() {
        assertEquals("{\"a\":\"1\",\"b\":\"2\"}", HttpParamConverter.ofString(() -> "a=1&b=2"));
    }

    @Test
    public void testInitQueryParams() {
        Map<String, String> params = HttpParamConverter.initQueryParams("a=1&b=2");
        collector.checkThat(params,
                allOf(IsMapContaining.hasEntry("a", "1"),
                        IsMapContaining.hasEntry("b", "2")));
    }

    @Test
    public void testDecodeQueryParam() {
        assertEquals("a=1&b=2", HttpParamConverter.decodeQueryParam("a%3d1%26b%3d2"));

        try {
            HttpParamConverter.decodeQueryParam("%%");
        } catch (Throwable throwable) {
            assertThat(throwable.getMessage(), is("URLDecoder: Incomplete trailing escape (%) pattern"));
        }

    }

    @Test
    public void testToMap() {
        final MultiValueMap<String, String> args = new LinkedMultiValueMap<>();
        args.add("a", "1");
        args.add("a", "2");
        args.add("b", "3");
        String actual = HttpParamConverter.toMap(() -> args);
        assertEquals("{\"a\":\"1\",\"b\":\"3\"}", actual);
    }
}

