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
import org.apache.shenyu.plugin.mock.api.MockRequest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.matchesRegex;

public class GeneratorFactoryTest {

    @Test
    public void testDealRule() {
        String dealedContent = GeneratorFactory.dealRule("${phone}", null);
        assertThat(dealedContent, matchesRegex("^\"1[3-9]\\d{9}\"$"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"$5", "C:\\temp\\file", "say \"hello\"", "first\nsecond"})
    public void testPreserveGeneratedLiteralValues(final String value) {
        MockRequest request = MockRequest.Builder.builder()
                .queries(Collections.singletonMap("value", value)).build();

        String content = GeneratorFactory.dealRule("{\"value\":${expression|#req.queries['value']}}", request);

        assertThat(content, is("{\"value\":" + JsonUtils.toJson(value) + "}"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"a+b", "a?b", "a*b", "a^b", "a.b", "a\\b", "\\E", "$5"})
    public void testMatchPlaceholderLiterally(final String value) {
        String content = GeneratorFactory.dealRule("${expression|'" + value + "'}", null);

        assertThat(content, is(JsonUtils.toJson(value)));
    }

    @Test
    public void testReplaceRepeatedAndDistinctExpressions() {
        String content = GeneratorFactory.dealRule("[${expression|1+2},${expression|1+2},${expression|2*3}]", null);

        assertThat(content, is("[3,3,6]"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"${expression|(sdxc}", "${wrong_rule|123}"})
    public void testDealRuleWithWrongContent(final String content) {
        String dealedContent = GeneratorFactory.dealRule(content, null);
        assertThat(dealedContent, is("\"[#ERROR EXPRESSION#]\""));
    }
}
