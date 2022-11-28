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

        generator.parseRule("expression|T(java.time.LocalDate).now()");
        assertThat(generator.generate(), is(JsonUtils.toJson(LocalDate.now().toString())));

        generator.parseRule("expression|1==1");
        assertThat(generator.generate(), is("true"));
    }

    @Test
    public void testBoolGenerate() {
        generator.parseRule("expression|#bool()");
        String generate = generator.generate();
        assertThat(generate, in(Arrays.asList("true", "false")));

    }

    @Test
    public void testCurrentTimeGenerate() {
        generator.parseRule("expression|#current()");
        assertTrue(generator.generate().matches("^\"\\d{4}(-\\d{2}){2} \\d{2}(:\\d{2}){2}\"$"));
        generator.parseRule("expression|#current('YYYY-MM-dd')");
        assertTrue(generator.generate().matches("^\"\\d{4}(-\\d{2}){2}\"$"));
    }

    @Test
    public void testEmailTimeGenerate() {
        generator.parseRule("expression|#email()");
        assertNotNull(generator.generate());
    }

    @Test
    public void testEnStringGenerate() {
        int max = 10;
        int min = 5;
        generator.parseRule(String.format("expression|#en(%d,%d)", min, max));
        String enString = generator.generate();
        assertThat(enString, matchesRegex("\"[a-zA-Z]{" + min + "," + max + "}\""));
    }

    @Test
    public void testPhoneGenerate() {
        generator.parseRule("expression|#phone()");
        String phone = generator.generate();
        assertTrue(phone.matches("^\"1[3-9]\\d{9}\"$"));
    }

    @Test
    public void testRandomDoubleGenerate() {
        double min = 10.5;
        double max = 12.0;
        generator.parseRule(String.format("expression|#double(%f,%f)", min, max));
        String doubleValue = generator.generate();
        assertThat(Double.valueOf(doubleValue), allOf(greaterThanOrEqualTo(min), lessThanOrEqualTo(max)));

        generator.parseRule("expression|#double(10.5,12.0,'￥%.2f')");
        doubleValue = generator.generate();
        assertThat(Double.valueOf(doubleValue.substring(1)), allOf(greaterThanOrEqualTo(min), lessThanOrEqualTo(max)));
        assertThat(doubleValue, matchesRegex("^￥\\d+.\\d{2}$"));
    }

    @Test
    public void testRandomIntGenerate() {
        int min = 10;
        int max = 15;
        generator.parseRule(String.format("expression|#int(%d,%d)", min, max));
        String val = generator.generate();
        assertThat(Integer.valueOf(val), allOf(greaterThanOrEqualTo(min), lessThanOrEqualTo(max)));
    }

    @Test
    public void testRandomDataGenerate() {

        generator.parseRule("expression|#oneOf('shenyu','number',1)");
        String val = generator.generate();
        assertThat(val, oneOf("\"shenyu\"", "\"number\"", "1"));
    }

    @Test
    public void testZhDataGenerate() {

        int minLength = 10;
        int maxLength = 20;
        generator.parseRule(String.format("expression|#zh(%d,%d)", minLength, maxLength));
        String val = generator.generate();
        assertThat(val.length(), allOf(greaterThanOrEqualTo(minLength), lessThanOrEqualTo(maxLength)));
    }

    @Test
    public void testMatch() {
        assertTrue(generator.match("expression|23"));
        assertFalse(generator.match("expression"));
        assertFalse(generator.match("expression|"));
    }
}
