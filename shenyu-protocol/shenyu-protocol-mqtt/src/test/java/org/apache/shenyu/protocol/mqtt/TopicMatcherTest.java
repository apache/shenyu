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

package org.apache.shenyu.protocol.mqtt;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TopicMatcherTest {

    @Test
    void testExactMatch() {
        assertTrue(TopicMatcher.matches("sport/tennis/player1", "sport/tennis/player1"));
        assertFalse(TopicMatcher.matches("sport/tennis/player1", "sport/tennis/player2"));
    }

    @Test
    void testSingleLevelWildcard() {
        assertTrue(TopicMatcher.matches("sport/+/player1", "sport/tennis/player1"));
        assertTrue(TopicMatcher.matches("sport/+/player1", "sport/football/player1"));
        assertTrue(TopicMatcher.matches("+/tennis/+", "sport/tennis/player1"));
        assertFalse(TopicMatcher.matches("sport/+/player1", "sport/tennis/stadium/player1"));
        assertFalse(TopicMatcher.matches("sport/+", "sport"));
    }

    @Test
    void testMultiLevelWildcard() {
        assertTrue(TopicMatcher.matches("#", "sport/tennis/player1"));
        assertTrue(TopicMatcher.matches("#", "sport"));
        assertTrue(TopicMatcher.matches("sport/#", "sport"));
        assertTrue(TopicMatcher.matches("sport/#", "sport/tennis"));
        assertTrue(TopicMatcher.matches("sport/#", "sport/tennis/player1"));
        assertTrue(TopicMatcher.matches("sport/#", "sport/tennis/player1/ranking"));
        assertFalse(TopicMatcher.matches("sport#", "sport/tennis"));
        assertFalse(TopicMatcher.matches("sport/tennis#", "sport/tennis/player1"));
    }

    @Test
    void testMixedWildcards() {
        assertTrue(TopicMatcher.matches("+/tennis/#", "sport/tennis/player1"));
        assertTrue(TopicMatcher.matches("+/tennis/#", "sport/tennis/player1/ranking"));
        assertTrue(TopicMatcher.matches("sport/+/#", "sport/tennis/player1"));
        assertFalse(TopicMatcher.matches("+/tennis/#", "sport/football/player1"));
    }

    @Test
    void testDollarTopicNotMatchedByLeadingWildcard() {
        assertFalse(TopicMatcher.matches("#", "$SYS/broker/version"));
        assertFalse(TopicMatcher.matches("+", "$SYS"));
        assertFalse(TopicMatcher.matches("+/b", "$SYS/b"));
        assertTrue(TopicMatcher.matches("$SYS/#", "$SYS/broker/version"));
        assertTrue(TopicMatcher.matches("$SYS/+", "$SYS/broker"));
    }

    @Test
    void testNullInput() {
        assertFalse(TopicMatcher.matches(null, "topic"));
        assertFalse(TopicMatcher.matches("topic", null));
    }

    @Test
    void testValidFilter() {
        assertTrue(TopicMatcher.isValidFilter("sport/tennis/player1"));
        assertTrue(TopicMatcher.isValidFilter("sport/+/player1"));
        assertTrue(TopicMatcher.isValidFilter("+"));
        assertTrue(TopicMatcher.isValidFilter("#"));
        assertTrue(TopicMatcher.isValidFilter("sport/#"));
        assertTrue(TopicMatcher.isValidFilter("+/tennis/#"));
    }

    @Test
    void testInvalidFilter() {
        assertFalse(TopicMatcher.isValidFilter("sport#"));
        assertFalse(TopicMatcher.isValidFilter("sport/tennis#"));
        assertFalse(TopicMatcher.isValidFilter("#/sport"));
        assertFalse(TopicMatcher.isValidFilter("sport/#/ranking"));
        assertFalse(TopicMatcher.isValidFilter("sport+"));
        assertFalse(TopicMatcher.isValidFilter("+tennis"));
        assertFalse(TopicMatcher.isValidFilter("sport/+tennis"));
        assertFalse(TopicMatcher.isValidFilter(""));
        assertFalse(TopicMatcher.isValidFilter(null));
        assertFalse(TopicMatcher.isValidFilter("sport/" + (char) 0));
    }
}
