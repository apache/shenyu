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

package org.apache.shenyu.sdk.core.util;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import static java.lang.String.format;

/**
 * Utilities, typically copied in from guava, so as to avoid dependency conflicts.
 */
public final class Util {

    /**
     * UTF-8: eight-bit UCS Transformation Format.
     */
    public static final Charset UTF_8 = StandardCharsets.UTF_8;

    /**
     * checkArgument.
     *
     * @param expression expression
     * @param errorMessageTemplate errorMessageTemplate
     * @param errorMessageArgs errorMessageArgs
     */
    public static void checkArgument(final boolean expression,
                                     final String errorMessageTemplate,
                                     final Object... errorMessageArgs) {
        if (!expression) {
            throw new IllegalArgumentException(
                    format(errorMessageTemplate, errorMessageArgs));
        }
    }

    /**
     * If the provided String is not null or empty.
     *
     * @param value to evaluate.
     * @return true of the value is not null and not empty.
     */
    public static boolean isNotBlank(final String value) {
        return value != null && !value.isEmpty();
    }

    /**
     * If the provided String is null or empty.
     *
     * @param value to evaluate.
     * @return true if the value is null or empty.
     */
    public static boolean isBlank(final String value) {
        return value == null || value.isEmpty();
    }

    /**
     * checkNotNull.
     *
     * @param reference reference
     * @param errorMessageTemplate errorMessageTemplate
     * @param errorMessageArgs errorMessageArgs
     * @param <T> t
     * @return {@link T}
     */
    public static <T> T checkNotNull(final T reference,
                                     final String errorMessageTemplate,
                                     final Object... errorMessageArgs) {
        if (reference == null) {
            // If either of these parameters is null, the right thing happens anyway
            throw new NullPointerException(
                    format(errorMessageTemplate, errorMessageArgs));
        }
        return reference;
    }

    /**
     * checkState.
     *
     * @param expression expression
     * @param errorMessageTemplate errorMessageTemplate
     * @param errorMessageArgs errorMessageArgs
     */
    public static void checkState(final boolean expression,
                                  final String errorMessageTemplate,
                                  final Object... errorMessageArgs) {
        if (!expression) {
            throw new IllegalStateException(
                    format(errorMessageTemplate, errorMessageArgs));
        }
    }

    /**
     * Identifies a method as a default instance method.
     * @param method method
     * @return bool
     */
    public static boolean isDefault(final Method method) {
        // Default methods are public non-abstract, non-synthetic, and non-static instance methods
        // declared in an interface.
        // method.isDefault() is not sufficient for our usage as it does not check
        // for synthetic methods. As a result, it picks up overridden methods as well as actual default
        // methods.
        return ((method.getModifiers()
                & (Modifier.ABSTRACT | Modifier.PUBLIC | Modifier.STATIC | 0x00001000)) == Modifier.PUBLIC)
                && method.getDeclaringClass().isInterface();
    }

}
