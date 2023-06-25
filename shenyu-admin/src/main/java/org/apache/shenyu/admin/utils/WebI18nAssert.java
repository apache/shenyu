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

package org.apache.shenyu.admin.utils;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.WebI18nException;
import org.springframework.util.CollectionUtils;

import java.util.Collection;
import java.util.Objects;

/**
 * Assert.<br>
 * Support for assertion tools in web environments，
 */
public final class WebI18nAssert {
    
    private WebI18nAssert() {
    }
    
    /**
     * assert obj is not null.
     *
     * @param obj     obj
     * @param message error message
     * @param objects objects
     */
    public static void notNull(final Object obj, final String message, final Object... objects) {
        isTrue(Objects.nonNull(obj), message, objects);
    }
    
    /**
     * assert obj is null.
     *
     * @param obj     obj
     * @param message error message
     * @param objects objects
     */
    public static void isNull(final Object obj, final String message, final Object... objects) {
        isTrue(Objects.isNull(obj), message, objects);
    }
    
    /**
     * assert string is not black.
     *
     * @param str     string
     * @param message error message
     * @param objects objects
     */
    public static void notBlack(final String str, final String message, final Object... objects) {
        isTrue(StringUtils.isNoneBlank(str), message, objects);
    }
    
    /**
     * assert collection is not empty.
     *
     * @param collection obj
     * @param message    error message
     * @param objects    objects
     */
    public static void notEmpty(final Collection<?> collection, final String message, final Object... objects) {
        isTrue(!CollectionUtils.isEmpty(collection), message, objects);
    }
    
    /**
     * assert test is true.
     *
     * @param test    string
     * @param message error message
     * @param objects objects
     */
    public static void isTrue(final Boolean test, final String message, final Object... objects) {
        if (!Boolean.TRUE.equals(test)) {
            fail(message, objects);
        }
    }
    
    /**
     * fail.
     *
     * @param message message
     * @param objects objects
     */
    public static void fail(final String message, final Object... objects) {
        throw new WebI18nException(message, objects);
    }
}
