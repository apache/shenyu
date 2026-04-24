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

import org.apache.shenyu.admin.exception.WebI18nException;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test case for {@link WebI18nAssert}.
 */
public class WebI18nAssertTest {

    @Test
    void testNotNullWithValidObject() {
        assertDoesNotThrow(() -> {
            WebI18nAssert.notNull("test", "error message");
            WebI18nAssert.notNull(123, "error message");
            WebI18nAssert.notNull(new Object(), "error message");
        });
    }

    @Test
    void testNotNullWithNullObject() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.notNull(null, "error message"));
    }

    @Test
    void testNotNullWithNullObjectAndParams() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.notNull(null, "error message with param: %s", "param1"));
    }

    @Test
    void testIsNullWithNullObject() {
        assertDoesNotThrow(() -> WebI18nAssert.isNull(null, "error message"));
    }

    @Test
    void testIsNullWithValidObject() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.isNull("test", "error message"));
    }

    @Test
    void testIsNullWithValidObjectAndParams() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.isNull(123, "error message with param: %s", "param1"));
    }

    @Test
    void testNotBlackWithValidString() {
        assertDoesNotThrow(() -> {
            WebI18nAssert.notBlack("test", "error message");
            WebI18nAssert.notBlack(" a ", "error message");
            WebI18nAssert.notBlack("test string", "error message");
        });
    }

    @Test
    void testNotBlackWithNullString() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.notBlack(null, "error message"));
    }

    @Test
    void testNotBlackWithEmptyString() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.notBlack("", "error message"));
    }

    @Test
    void testNotBlackWithBlankString() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.notBlack("   ", "error message"));
    }

    @Test
    void testNotBlackWithBlankStringAndParams() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.notBlack("", "error message with param: %s", "param1"));
    }

    @Test
    void testNotEmptyWithValidCollection() {
        assertDoesNotThrow(() -> {
            List<String> list = Arrays.asList("item1", "item2");
            WebI18nAssert.notEmpty(list, "error message");

            Collection<Integer> collection = new ArrayList<>();
            collection.add(1);
            WebI18nAssert.notEmpty(collection, "error message");
        });
    }

    @Test
    void testNotEmptyWithNullCollection() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.notEmpty(null, "error message"));
    }

    @Test
    void testNotEmptyWithEmptyCollection() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.notEmpty(Collections.emptyList(), "error message"));
    }

    @Test
    void testNotEmptyWithEmptyCollectionAndParams() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.notEmpty(new ArrayList<>(), "error message with param: %s", "param1"));
    }

    @Test
    void testIsTrueWithTrueBoolean() {
        assertDoesNotThrow(() -> {
            WebI18nAssert.isTrue(true, "error message");
            WebI18nAssert.isTrue(Boolean.TRUE, "error message");
        });
    }

    @Test
    void testIsTrueWithFalseBoolean() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.isTrue(false, "error message"));
    }

    @Test
    void testIsTrueWithNullBoolean() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.isTrue(null, "error message"));
    }

    @Test
    void testIsTrueWithFalseBooleanAndParams() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.isTrue(Boolean.FALSE, "error message with param: %s", "param1"));
    }

    @Test
    void testFail() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.fail("error message"));
    }

    @Test
    void testFailWithParams() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.fail("error message with param: %s", "param1"));
    }

    @Test
    void testFailWithMultipleParams() {
        assertThrows(WebI18nException.class, () -> WebI18nAssert.fail("error message with params: %s, %s, %d", "param1", "param2", 123));
    }

    @Test
    void testNotNullWithParams() {
        assertDoesNotThrow(() -> WebI18nAssert.notNull("test", "error message with param: %s", "param1"));
    }

    @Test
    void testIsNullWithParams() {
        assertDoesNotThrow(() -> WebI18nAssert.isNull(null, "error message with param: %s", "param1"));
    }

    @Test
    void testNotBlackWithParams() {
        assertDoesNotThrow(() -> WebI18nAssert.notBlack("test", "error message with param: %s", "param1"));
    }

    @Test
    void testNotEmptyWithParams() {
        assertDoesNotThrow(() -> {
            final List<String> list = Collections.singletonList("item");
            WebI18nAssert.notEmpty(list, "error message with param: %s", "param1");
        });
    }

    @Test
    void testIsTrueWithParams() {
        assertDoesNotThrow(() -> WebI18nAssert.isTrue(true, "error message with param: %s", "param1"));
    }
}
