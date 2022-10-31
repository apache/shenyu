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
import org.apache.shenyu.admin.exception.ResourceNotFoundException;
import org.apache.shenyu.admin.exception.ValidFailException;
import org.springframework.util.CollectionUtils;

import java.util.Collection;
import java.util.Objects;

/**
 * Assert.
 */
public final class Assert {
    
    private Assert() {
    }
    
    /**
     * assert obj is not null.
     *
     * @param obj     obj
     * @param message error message
     */
    public static void notNull(final Object obj, final String message) {
        isTrue(Objects.nonNull(obj), message);
    }
    
    /**
     * assert obj is null.
     *
     * @param obj     obj
     * @param message error message
     */
    public static void isNull(final Object obj, final String message) {
        isTrue(Objects.isNull(obj), message);
    }
    
    /**
     * assert string is not black.
     *
     * @param str     string
     * @param message error message
     */
    public static void notBlack(final String str, final String message) {
        isTrue(StringUtils.isNoneBlank(str), message);
    }
    
    /**
     * assert collection is not empty.
     *
     * @param collection obj
     * @param message    error message
     */
    public static void notEmpty(final Collection<?> collection, final String message) {
        isTrue(!CollectionUtils.isEmpty(collection), message);
    }
    
    /**
     * assert test is true.
     *
     * @param test    string
     * @param message error message
     */
    public static void isTrue(final Boolean test, final String message) {
        if (!Boolean.TRUE.equals(test)) {
            throw new ValidFailException(message);
        }
    }
    
    /**
     * throw ResourceNotFoundException with default message.
     *
     * @param e exception
     */
    public static void throwException(final Exception e) {
        throw new ResourceNotFoundException("the validation ExistProviderMethod invoked error", e);
    }
}
