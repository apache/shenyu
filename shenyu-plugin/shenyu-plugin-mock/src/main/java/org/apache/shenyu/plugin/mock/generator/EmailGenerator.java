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

import org.apache.shenyu.spi.Join;

import static org.apache.shenyu.plugin.mock.util.RandomUtil.randomInt;
import static org.apache.shenyu.plugin.mock.util.RandomUtil.randomLowerLetterString;

/**
 * Random email address generator.
 */
@Join
public class EmailGenerator implements Generator<String> {
    
    private static final String[] DOMAIN_SUFFIX = {"com", "org", "cn", "com.cn", "top", "edu", "io"};
    
    @Override
    public String getName() {
        return "email";
    }
    
    @Override
    public String generate() {
        return String.format("%s@%s.%s",
            randomLowerLetterString(randomInt(5, 10)),
            randomLowerLetterString(randomInt(3, 8)),
            DOMAIN_SUFFIX[randomInt(0, DOMAIN_SUFFIX.length - 1)]);
    }
    
    @Override
    public int getParamSize() {
        return 0;
    }
    
    @Override
    public boolean match(final String rule) {
        return rule.matches("^email$");
    }
    
    @Override
    public String[] getPrefixAndSuffix() {
        return new String[]{"\"", "\""};
    }
}
