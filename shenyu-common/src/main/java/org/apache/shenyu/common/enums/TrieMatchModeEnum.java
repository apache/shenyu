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

package org.apache.shenyu.common.enums;

import java.util.Arrays;

/**
 * Shenyu match mode event.
 */
public enum TrieMatchModeEnum {
    /**
     * ant path match.
     */
    ANT_PATH_MATCH("antPathMatch"),

    /**
     * path pattern.
     */
    PATH_PATTERN("pathPattern");

    private final String matchMode;

    TrieMatchModeEnum(final String matchMode) {
        this.matchMode = matchMode;
    }

    /**
     * get trie match mode.
     *
     * @return match mode
     */
    public String getMatchMode() {
        return matchMode;
    }
    
    /**
     * get {@linkplain TrieMatchModeEnum} by match mode.
     *
     * @param matchMode match mode
     * @return {@linkplain TrieMatchModeEnum}
     */
    public static TrieMatchModeEnum acquireTrieMatch(final String matchMode) {
        return Arrays.stream(TrieMatchModeEnum.values())
                .filter(e -> e.getMatchMode().equals(matchMode))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Shenyu trie match mode is error, match mode:" + matchMode));
    }
}
