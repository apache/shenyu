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

package org.apache.shenyu.plugin.logging.desensitize.api.matcher;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

@ExtendWith(MockitoExtension.class)
class KeyWordMatchTest {

    private KeyWordMatch keyWordMatch;

    @BeforeEach
    public void setUp() {
        Set<String> set = new HashSet<>();
        set.add("name");
        set.add("TesT");
        set.add("dsadsader");
        keyWordMatch = new KeyWordMatch(set);
    }

    @Test
    public void matches() {
        Assertions.assertTrue(keyWordMatch.matches("name"));
        Assertions.assertTrue(keyWordMatch.matches("test"));
        Assertions.assertFalse(keyWordMatch.matches("dsaer"));
    }

    @Test
    public void matchesKeywordsContainingRegexMetacharacters() {
        Set<String> set = new HashSet<>();
        set.add("a.b");
        set.add("ab[secret]yz");
        KeyWordMatch match = new KeyWordMatch(set);

        Assertions.assertTrue(match.matches("a.b"));
        Assertions.assertFalse(match.matches("axb"));
        Assertions.assertTrue(match.matches("ab[secret]yz"));
        Assertions.assertTrue(match.matches("ab[other]yz"));
    }

    @Test
    public void matchesShouldNotAcceptTheEmptyKeyword() {
        Assertions.assertFalse(keyWordMatch.matches(""), "an empty key must not be treated as a sensitive keyword");
    }

    @Test
    public void matchesShouldIgnoreBlankTokensFromSplitKeywords() {
        // keywords.split(";") on a value like "password;;name" contributes empty tokens
        Set<String> mixed = new HashSet<>(Arrays.asList("password", ""));
        KeyWordMatch match = new KeyWordMatch(mixed);
        Assertions.assertFalse(match.matches(""), "an empty token must not recreate the empty-alternative bug");
        Assertions.assertTrue(match.matches("password"));

        Set<String> leadingBlank = new HashSet<>(Arrays.asList("", "name"));
        Assertions.assertFalse(new KeyWordMatch(leadingBlank).matches(""));
    }

    @Test
    public void matchesShouldNeverMatchWhenAllTokensAreBlank() {
        Set<String> allBlank = new HashSet<>(Arrays.asList("", "   "));
        KeyWordMatch match = new KeyWordMatch(allBlank);
        Assertions.assertFalse(match.matches(""));
        Assertions.assertFalse(match.matches("anything"));
    }

    @Test
    public void matchesShouldKeepKeywordSemantics() {
        Set<String> set = new HashSet<>();
        set.add("password");
        KeyWordMatch match = new KeyWordMatch(set);

        Assertions.assertTrue(match.matches("password"));
        Assertions.assertTrue(match.matches("PASSWORD"));
        Assertions.assertFalse(match.matches("userName"));
    }

}
