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
}
