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

package org.apache.shenyu.plugin.ai.sensitive.word.ac;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Test cases for {@link AhoCorasick}.
 */
public final class AhoCorasickTest {

    @Test
    public void testMatchSingleWord() {
        AhoCorasick automaton = AhoCorasick.of(Collections.singletonList("sensitive"));
        assertThat(automaton.search("this is a sensitive word")).containsExactly("sensitive");
    }

    @Test
    public void testMatchEveryWordOfTheText() {
        AhoCorasick automaton = AhoCorasick.of(Arrays.asList("sensitive", "word", "spam"));
        assertThat(automaton.search("a sensitive word and a spam")).containsExactlyInAnyOrder("sensitive", "word", "spam");
    }

    @Test
    public void testMatchNestedWords() {
        AhoCorasick automaton = AhoCorasick.of(Arrays.asList("abc", "bc", "c"));
        assertThat(automaton.search("abc")).containsExactlyInAnyOrder("abc", "bc", "c");
    }

    @Test
    public void testMatchOverlappingWords() {
        AhoCorasick automaton = AhoCorasick.of(Arrays.asList("中国", "中国银行"));
        assertThat(automaton.search("中国银行")).containsExactlyInAnyOrder("中国", "中国银行");
    }

    @Test
    public void testMatchSuffixWords() {
        AhoCorasick automaton = AhoCorasick.of(Arrays.asList("敏感词", "词"));
        assertThat(automaton.search("这里有敏感词")).containsExactlyInAnyOrder("敏感词", "词");
    }

    @Test
    public void testFailureLinksAreFollowed() {
        AhoCorasick automaton = AhoCorasick.of(Arrays.asList("he", "she", "his", "hers"));
        assertThat(automaton.search("ushers")).containsExactlyInAnyOrder("she", "he", "hers");
    }

    @Test
    public void testMatchAtTextBoundaries() {
        AhoCorasick automaton = AhoCorasick.of(Collections.singletonList("ab"));
        assertThat(automaton.search("abxxab")).containsExactly("ab");
        assertThat(automaton.search("ab")).containsExactly("ab");
        assertThat(automaton.search("a")).isEmpty();
    }

    @Test
    public void testWordIsReportedOnce() {
        AhoCorasick automaton = AhoCorasick.of(Collections.singletonList("spam"));
        assertThat(automaton.search("spam spam spam")).containsExactly("spam");
    }

    @Test
    public void testMatchCjkText() {
        AhoCorasick automaton = AhoCorasick.of(Arrays.asList("违规", "敏感"));
        assertThat(automaton.search("这段内容违规而且敏感")).containsExactlyInAnyOrder("违规", "敏感");
    }

    @Test
    public void testNoMatch() {
        AhoCorasick automaton = AhoCorasick.of(Arrays.asList("sensitive", "spam"));
        assertThat(automaton.search("a clean text")).isEmpty();
    }

    @Test
    public void testEmptyDictionary() {
        assertThat(AhoCorasick.of(Collections.emptyList()).search("anything")).isEmpty();
        assertThat(AhoCorasick.of(null).search("anything")).isEmpty();
        assertThat(AhoCorasick.empty().search("anything")).isEmpty();
    }

    @Test
    public void testBlankAndNullWordsAreIgnored() {
        AhoCorasick automaton = AhoCorasick.of(Arrays.asList(" ", "", null, "  hit  "));
        assertThat(automaton.search("a hit")).containsExactly("hit");
        assertThat(automaton.search("nothing")).isEmpty();
    }

    @Test
    public void testNullAndEmptyText() {
        AhoCorasick automaton = AhoCorasick.of(Collections.singletonList("sensitive"));
        assertThat(automaton.search(null)).isEmpty();
        assertThat(automaton.search("")).isEmpty();
    }

    @Test
    public void testLongText() {
        StringBuilder text = new StringBuilder();
        for (int i = 0; i < 10000; i++) {
            text.append("filler");
        }
        text.append("sensitive");
        AhoCorasick automaton = AhoCorasick.of(Arrays.asList("sensitive", "filler"));
        assertThat(automaton.search(text.toString())).containsExactlyInAnyOrder("filler", "sensitive");
    }
}
