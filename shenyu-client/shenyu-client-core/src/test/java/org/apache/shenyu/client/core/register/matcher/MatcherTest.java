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

package org.apache.shenyu.client.core.register.matcher;

import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class MatcherTest {

    @Test
    public void testMatch() {
        Matcher<String> matcher = s -> s.startsWith("shen");
        assertTrue(matcher.match("shenyu"));
        assertFalse(matcher.match("apache"));
    }

    @Test
    public void testAnd() {
        Matcher<String> startsWithShen = s -> s.startsWith("shen");
        Matcher<String> endsWithYu = s -> s.endsWith("yu");
        assertTrue(startsWithShen.and(endsWithYu).match("shenyu"));
        assertFalse(startsWithShen.and(endsWithYu).match("shenyu-java"));
        assertFalse(startsWithShen.and(endsWithYu).match("apache-yu"));
    }

    @Test
    public void testAndShortCircuit() {
        AtomicInteger evaluations = new AtomicInteger();
        Matcher<String> first = s -> false;
        Matcher<String> second = s -> {
            evaluations.incrementAndGet();
            return true;
        };
        assertFalse(first.and(second).match("shenyu"));
        assertTrue(evaluations.get() == 0, "second matcher should not be evaluated when first is false");
    }

    @Test
    public void testOr() {
        Matcher<String> startsWithShen = s -> s.startsWith("shen");
        Matcher<String> endsWithYu = s -> s.endsWith("yu");
        assertTrue(startsWithShen.or(endsWithYu).match("shenyu-java"));
        assertTrue(startsWithShen.or(endsWithYu).match("apache-yu"));
        assertFalse(startsWithShen.or(endsWithYu).match("apache"));
    }

    @Test
    public void testOrShortCircuit() {
        AtomicInteger evaluations = new AtomicInteger();
        Matcher<String> first = s -> true;
        Matcher<String> second = s -> {
            evaluations.incrementAndGet();
            return true;
        };
        assertTrue(first.or(second).match("shenyu"));
        assertTrue(evaluations.get() == 0, "second matcher should not be evaluated when first is true");
    }

    @Test
    public void testNegate() {
        Matcher<String> matcher = s -> s.isEmpty();
        assertFalse(matcher.negate().match(""));
        assertTrue(matcher.negate().match("shenyu"));
    }

    @Test
    public void testNot() {
        Matcher<String> matcher = s -> s.isEmpty();
        Matcher<String> not = Matcher.not(matcher);
        assertNotNull(not);
        assertFalse(not.match(""));
        assertTrue(not.match("shenyu"));
    }

    @Test
    public void testAndWithNullThrowsNpe() {
        Matcher<String> matcher = s -> true;
        assertThrows(NullPointerException.class, () -> matcher.and(null));
    }

    @Test
    public void testOrWithNullThrowsNpe() {
        Matcher<String> matcher = s -> true;
        assertThrows(NullPointerException.class, () -> matcher.or(null));
    }

    @Test
    public void testNotWithNullThrowsNpe() {
        assertThrows(NullPointerException.class, () -> Matcher.not(null));
    }
}
