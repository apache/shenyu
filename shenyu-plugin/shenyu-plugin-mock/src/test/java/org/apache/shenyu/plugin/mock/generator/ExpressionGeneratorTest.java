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

package org.apache.shenyu.plugin.mock.generator;

import com.google.common.collect.ImmutableMap;
import org.apache.shenyu.common.enums.HttpMethodEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.mock.api.MockRequest;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.util.Arrays;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.in;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.hamcrest.Matchers.matchesRegex;
import static org.hamcrest.Matchers.oneOf;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ExpressionGeneratorTest {

    private static MockRequest mockRequest;

    private final ExpressionGenerator generator = new ExpressionGenerator();

    @BeforeAll
    public static void setUp() {
        byte[] body = JsonUtils.toJson(ImmutableMap.of(
                "name", "shenyu",
                "id", 1234L,
                "address", ImmutableMap.of("country", "CHINA")))
                .getBytes(StandardCharsets.UTF_8);

        mockRequest = MockRequest.Builder.builder()
                .uri("getName")
                .headers(ImmutableMap.of("header_name", "header_value"))
                .method(HttpMethodEnum.GET.getName())
                .queries(ImmutableMap.of("query_name", "query_value"))
                .body(body)
                .build();

    }

    @Test
    public void testGenerate() {

        assertThat(generator.generate("expression|T(java.time.LocalDate).now()", mockRequest),
                is(JsonUtils.toJson(LocalDate.now().toString())));

        assertThat(generator.generate("expression|1==1", mockRequest),
                is("true"));
    }

    @Test
    public void testBoolGenerate() {
        String generate = generator.generate("expression|#bool()", mockRequest);
        assertThat(generate, in(Arrays.asList("true", "false")));

    }

    @Test
    public void testCurrentTimeGenerate() {
        assertThat(generator.generate("expression|#current()", mockRequest),
                matchesRegex("^\"\\d{4}(-\\d{2}){2} \\d{2}(:\\d{2}){2}\"$"));

        assertThat(generator.generate("expression|#current('YYYY-MM-dd')", mockRequest),
                matchesRegex("^\"\\d{4}(-\\d{2}){2}\"$"));
    }

    @Test
    public void testEmailTimeGenerate() {
        assertNotNull(generator.generate("expression|#email()", mockRequest));
    }

    @Test
    public void testEnStringGenerate() {
        int max = 10;
        int min = 5;
        String enString = generator.generate(String.format("expression|#en(%d,%d)", min, max), mockRequest);
        assertThat(enString, matchesRegex("\"[a-zA-Z]{" + min + "," + max + "}\""));
    }

    @Test
    public void testPhoneGenerate() {
        String phone = generator.generate("expression|#phone()", mockRequest);
        assertTrue(phone.matches("^\"1[3-9]\\d{9}\"$"));
    }

    @Test
    public void testRandomDoubleGenerate() {
        double min = 10.5;
        double max = 12.0;
        String doubleValue = generator.generate(String.format("expression|#double(%f,%f)", min, max), mockRequest);
        assertThat(Double.valueOf(doubleValue), allOf(greaterThanOrEqualTo(min), lessThanOrEqualTo(max)));

        doubleValue = generator.generate("expression|#double(10.5,12.0,'￥%.2f')", mockRequest);
        assertThat(Double.valueOf(doubleValue.substring(1)), allOf(greaterThanOrEqualTo(min), lessThanOrEqualTo(max)));
        assertThat(doubleValue, matchesRegex("^￥\\d+.\\d{2}$"));
    }

    @Test
    public void testRandomIntGenerate() {
        int min = 10;
        int max = 15;
        String val = generator.generate(String.format("expression|#int(%d,%d)", min, max), mockRequest);
        assertThat(Integer.valueOf(val), allOf(greaterThanOrEqualTo(min), lessThanOrEqualTo(max)));
    }

    @Test
    public void testRandomDataGenerate() {

        String val = generator.generate("expression|#oneOf('shenyu','number',1)", mockRequest);
        assertThat(val, oneOf("\"shenyu\"", "\"number\"", "1"));
    }

    @Test
    public void testArrayGenerate() {

        String val = generator.generate("expression|#array(#array('shenyu',2),2)", mockRequest);
        assertThat(val, is("[[\"shenyu\",\"shenyu\"],[\"shenyu\",\"shenyu\"]]"));

        val = generator.generate("expression|#array(#bool(),2)", mockRequest);
        assertThat(val, oneOf("[true,false]", "[false,true]", "[false,false]", "[true,true]"));

        val = generator.generate("expression|#array(#double(10.5,12.0,'%.2f'),2)", mockRequest);
        assertThat(val, Matchers.notNullValue());
    }

    @Test
    public void testGenerateDataFromReq() {

        assertThat(generator.generate("expression|#req.headers[header_name]", mockRequest),
                is("\"header_value\""));

        assertThat(generator.generate("expression|#req.method", mockRequest),
                is("\"get\""));

        assertThat(generator.generate("expression|#req.queries['query_name']", mockRequest),
                is("\"query_value\""));

        assertThat(generator.generate("expression|#req.queries.query_name", mockRequest),
                is("\"query_value\""));

        assertThat(generator.generate("expression|#req.json.id", mockRequest),
                is("1234"));

        assertThat(generator.generate("expression|#req.json.name", mockRequest),
                is("\"shenyu\""));

        assertThat(generator.generate("expression|#req.uri", mockRequest),
                is("\"getName\""));

        assertThat(generator.generate("expression|#req.json.address.country", mockRequest),
                is("\"CHINA\""));
    }

    @Test
    public void testZhDataGenerate() {

        int minLength = 10;
        int maxLength = 20;
        String val = generator.generate(String.format("expression|#zh(%d,%d)", minLength, maxLength), mockRequest);
        assertThat(val.length(), allOf(greaterThanOrEqualTo(minLength), lessThanOrEqualTo(maxLength)));
    }

    @Test
    public void testMatch() {
        assertTrue(generator.match("expression|23"));
        assertFalse(generator.match("expression"));
        assertFalse(generator.match("expression|"));
    }
}
