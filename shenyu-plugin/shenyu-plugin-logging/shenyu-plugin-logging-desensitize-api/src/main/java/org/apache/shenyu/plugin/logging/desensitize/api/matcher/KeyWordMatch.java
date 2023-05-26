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

import java.util.Set;
import java.util.regex.Pattern;

/**
 * according input keyWord generate regex.
 */
public class KeyWordMatch {

    private final Pattern p;

    /**
     * generate regex.
     *
     * @param keyWordSet keyWord set
     */
    public KeyWordMatch(final Set<String> keyWordSet) {
        StringBuilder sb = new StringBuilder();
        keyWordSet.forEach(tempKeyWord -> {
            sb.append("(?i)");
            if (tempKeyWord.length() <= 6) {
                sb.append(tempKeyWord);
            } else {
                sb.append("^" + tempKeyWord.substring(0, 3) + "(.*?)" + tempKeyWord.substring(tempKeyWord.length() - 3) + "$");
            }
            sb.append("||");
        });
        p = Pattern.compile(sb.toString());
    }

    /**
     * according regex match keyWord.
     *
     * @param keyWord keyWord
     * @return isMatch
     */
    public boolean matches(final String keyWord) {
        return p.matcher(keyWord).matches();
    }
}
