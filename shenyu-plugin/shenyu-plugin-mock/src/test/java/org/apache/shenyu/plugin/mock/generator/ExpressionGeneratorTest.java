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

import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ExpressionGeneratorTest {

    private final ExpressionGenerator generator = new ExpressionGenerator();

    @Test
    public void testGenerate() {

        generator.parseRule("expression|T(java.time.LocalDate).now()");
        assertThat(generator.generate(), is(LocalDate.now().toString()));

        generator.parseRule("expression|T(org.apache.shenyu.plugin.mock.util.RandomUtil).randomInt(1,100)");
        assertThat(Integer.valueOf(generator.generate()),
                is(Matchers.allOf(greaterThanOrEqualTo(1), lessThanOrEqualTo(100))));
    }

    @Test
    public void testGenerateWithWrongExpression() {
        generator.parseRule("expression|(sdxc");
        assertThat(generator.generate(), is("Wrong expression!!!,please check!!!"));
    }

    @Test
    public void testMatch() {
        assertTrue(generator.match("expression|23"));

        assertFalse(generator.match("expression"));
        assertFalse(generator.match("expression|"));
    }
}
