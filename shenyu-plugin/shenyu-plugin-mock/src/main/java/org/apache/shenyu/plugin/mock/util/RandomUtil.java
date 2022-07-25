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

package org.apache.shenyu.plugin.mock.util;

import org.apache.commons.lang3.RandomStringUtils;

import java.util.concurrent.ThreadLocalRandom;

/**
 * Random Util.
 */
public class RandomUtil {
    
    /**
     * Randomly generate integers.
     *
     * @param min min included
     * @param max max included
     * @return random int
     */
    public static int randomInt(final int min, final int max) {
        ThreadLocalRandom random = ThreadLocalRandom.current();
        return random.nextInt(max - min + 1) + min;
    }
    
    /**
     * Generate a  string containing only lowercase letters.
     *
     * @param length length
     * @return string
     */
    public static String randomLowerLetterString(final int length) {
        return RandomStringUtils.random(length, 96, 123, true, false);
    }
}
