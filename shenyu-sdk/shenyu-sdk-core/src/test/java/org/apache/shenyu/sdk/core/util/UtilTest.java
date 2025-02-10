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
import java.util.Objects;
import java.util.stream.Stream;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test for {@link Util}.
 */
class UtilTest {

    @ParameterizedTest
    @MethodSource("argumentProvider")
    void testCheckArgument(
        final boolean expression, final String errorMessageTemplate, final Object[] errorMessageArgs, final Class<? extends Exception> expectedException) {
        if (Objects.isNull(expectedException)) {
            Util.checkArgument(expression, errorMessageTemplate, errorMessageArgs);
        } else {
            Exception exception = assertThrows(expectedException, () ->
                Util.checkArgument(expression, errorMessageTemplate, errorMessageArgs));
            assertEquals(String.format(errorMessageTemplate, errorMessageArgs), exception.getMessage());
        }
    }

    @ParameterizedTest
    @MethodSource("stringProvider")
    void testIsNotBlank(final String value, final boolean expectedResult) {
        boolean result = Util.isNotBlank(value);
        assertEquals(expectedResult, result);
    }

    @ParameterizedTest
    @MethodSource("stringProvider")
    void testIsBlank(final String value, final boolean expectedResult) {
        boolean result = Util.isBlank(value);
        assertEquals(!expectedResult, result);
    }

    @ParameterizedTest
    @MethodSource("referenceAndErrorMessageProvider")
    <T> void testCheckNotNull(
        final T reference, final String errorMessageTemplate, final Object[] errorMessageArgs, final boolean shouldThrowException) {
        if (shouldThrowException) {
            assertThrows(
                NullPointerException.class,
                () -> Util.checkNotNull(reference, errorMessageTemplate, errorMessageArgs)
            );
        } else {
            T result = Util.checkNotNull(reference, errorMessageTemplate, errorMessageArgs);
            assertEquals(reference, result);
        }
    }

    @ParameterizedTest
    @MethodSource("stateProvider")
    void testCheckState(
        final boolean expression, final String errorMessageTemplate, final Object[] errorMessageArgs, final String expectedExceptionName) {
        Class<? extends Exception> expectedException = null;

        if (Objects.nonNull(expectedExceptionName)) {
            try {
                expectedException = (Class<? extends Exception>) Class.forName(expectedExceptionName);
            } catch (ClassNotFoundException e) {
                throw new IllegalArgumentException("Invalid exception class: " + expectedExceptionName);
            }
        }

        if (Objects.isNull(expectedException)) {
            Util.checkState(expression, errorMessageTemplate, errorMessageArgs);
        } else {
            Exception exception = assertThrows(expectedException, () ->
                Util.checkState(expression, errorMessageTemplate, errorMessageArgs));
            assertEquals(String.format(errorMessageTemplate, errorMessageArgs), exception.getMessage());
        }
    }

    @ParameterizedTest
    @MethodSource("methodProvider")
    void testIsDefault(final Method method, final boolean expectedResult) {
        boolean actualResult = Util.isDefault(method);
        assertEquals(expectedResult, actualResult);
    }

    private static Stream<Object[]> argumentProvider() {
        return Stream.of(
            new Object[]{true, "Error message", new Object[]{}, null},
            new Object[]{false, "Error message", new Object[]{}, IllegalArgumentException.class},
            new Object[]{false, "Error message: %s", new Object[]{"Invalid input"}, IllegalArgumentException.class}
        );
    }

    private static Stream<Object[]> stringProvider() {
        return Stream.of(
            new Object[]{"shenyu", true},
            new Object[]{"", false},
            new Object[]{null, false}
        );
    }

    private static Stream<Object[]> referenceAndErrorMessageProvider() {
        return Stream.of(
            new Object[]{"Hello", "Error message", new Object[]{}, false},
            new Object[]{123, "Error message with args: %s %s", new Object[]{"arg1", "arg2"}, false},
            new Object[]{null, "Error message", new Object[]{}, true},
            new Object[]{null, "Error message with args: %s %s", new Object[]{"arg1", "arg2"}, true}
        );
    }

    private static Stream<Arguments> stateProvider() {
        return Stream.of(
            Arguments.of(true, "", new Object[]{}, null),
            Arguments.of(false, "Error message", new Object[]{}, IllegalStateException.class.getName()),
            Arguments.of(false, "Error message: %s", new Object[]{"Invalid input"}, IllegalStateException.class.getName())
        );
    }

    private static Stream<Object[]> methodProvider() {
        return Stream.of(
            new Object[]{getPublicNonAbstractMethod(), false},
            new Object[]{getPublicStaticMethod(), false},
            new Object[]{getDefaultMethod(), true}
        );
    }

    private static Method getPublicNonAbstractMethod() {
        try {
            return MockInterface.class.getMethod("publicNonAbstractMethod");
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    private static Method getPublicStaticMethod() {
        try {
            return MockInterface.class.getMethod("publicStaticMethod");
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    private static Method getDefaultMethod() {
        try {
            return MockInterface.class.getMethod("defaultMethod");
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    public interface MockInterface {
        void publicNonAbstractMethod();

        default void defaultMethod() {
        }

        static void publicStaticMethod() {
        }

    }

}
