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

import org.apache.shenyu.common.utils.JsonUtils;
import org.junit.jupiter.api.Test;

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

    private final ExpressionGenerator generator = new ExpressionGenerator();

    @Test
    public void testGenerate() {

        assertThat(generator.generate("expression|T(java.time.LocalDate).now()"),
                is(JsonUtils.toJson(LocalDate.now().toString())));

        assertThat(generator.generate("expression|1==1"),
                is("true"));
    }

    @Test
    public void testBoolGenerate() {
        String generate = generator.generate("expression|#bool()");
        assertThat(generate, in(Arrays.asList("true", "false")));

    }

    @Test
    public void testCurrentTimeGenerate() {
        assertThat(generator.generate("expression|#current()"),
                matchesRegex("^\"\\d{4}(-\\d{2}){2} \\d{2}(:\\d{2}){2}\"$"));

        generator.generate("expression|#current('YYYY-MM-dd')");
        assertThat(generator.generate("expression|#current('YYYY-MM-dd')"),
                matchesRegex("^\"\\d{4}(-\\d{2}){2}\"$"));
    }

    @Test
    public void testEmailTimeGenerate() {
        assertNotNull(generator.generate("expression|#email()"));
    }

    @Test
    public void testEnStringGenerate() {
        int max = 10;
        int min = 5;
        String enString = generator.generate(String.format("expression|#en(%d,%d)", min, max));
        assertThat(enString, matchesRegex("\"[a-zA-Z]{" + min + "," + max + "}\""));
    }

    @Test
    public void testPhoneGenerate() {
        String phone = generator.generate("expression|#phone()");
        assertTrue(phone.matches("^\"1[3-9]\\d{9}\"$"));
    }

    @Test
    public void testRandomDoubleGenerate() {
        double min = 10.5;
        double max = 12.0;
        String doubleValue = generator.generate(String.format("expression|#double(%f,%f)", min, max));
        assertThat(Double.valueOf(doubleValue), allOf(greaterThanOrEqualTo(min), lessThanOrEqualTo(max)));

        doubleValue = generator.generate("expression|#double(10.5,12.0,'￥%.2f')");
        assertThat(Double.valueOf(doubleValue.substring(1)), allOf(greaterThanOrEqualTo(min), lessThanOrEqualTo(max)));
        assertThat(doubleValue, matchesRegex("^￥\\d+.\\d{2}$"));
    }

    @Test
    public void testRandomIntGenerate() {
        int min = 10;
        int max = 15;
        String val = generator.generate(String.format("expression|#int(%d,%d)", min, max));
        assertThat(Integer.valueOf(val), allOf(greaterThanOrEqualTo(min), lessThanOrEqualTo(max)));
    }

    @Test
    public void testRandomDataGenerate() {

        String val = generator.generate("expression|#oneOf('shenyu','number',1)");
        assertThat(val, oneOf("\"shenyu\"", "\"number\"", "1"));
    }

    @Test
    public void testZhDataGenerate() {

        int minLength = 10;
        int maxLength = 20;
        String val = generator.generate(String.format("expression|#zh(%d,%d)", minLength, maxLength));
        assertThat(val.length(), allOf(greaterThanOrEqualTo(minLength), lessThanOrEqualTo(maxLength)));
    }

    @Test
    public void testMatch() {
        assertTrue(generator.match("expression|23"));
        assertFalse(generator.match("expression"));
        assertFalse(generator.match("expression|"));
    }
}
