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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

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
    @ValueSource(strings = {"${expression|(sdxc}", "${wrong_rule|123}"})
    public void testDealRuleWithWrongContent(final String content) {
        String dealedContent = GeneratorFactory.dealRule(content, null);
        assertThat(dealedContent, is("\"[#ERROR EXPRESSION#]\""));
    }
}
