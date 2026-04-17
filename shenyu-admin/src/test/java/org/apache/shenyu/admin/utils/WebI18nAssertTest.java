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
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Test case for {@link WebI18nAssert}.
 */
public class WebI18nAssertTest {

    @Test
    public void testNotNullWithValidObject() {
        // Should not throw exception
        WebI18nAssert.notNull("test", "error message");
        WebI18nAssert.notNull(123, "error message");
        WebI18nAssert.notNull(new Object(), "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testNotNullWithNullObject() {
        WebI18nAssert.notNull(null, "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testNotNullWithNullObjectAndParams() {
        WebI18nAssert.notNull(null, "error message with param: %s", "param1");
    }

    @Test
    public void testIsNullWithNullObject() {
        // Should not throw exception
        WebI18nAssert.isNull(null, "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testIsNullWithValidObject() {
        WebI18nAssert.isNull("test", "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testIsNullWithValidObjectAndParams() {
        WebI18nAssert.isNull(123, "error message with param: %s", "param1");
    }

    @Test
    public void testNotBlackWithValidString() {
        // Should not throw exception
        WebI18nAssert.notBlack("test", "error message");
        WebI18nAssert.notBlack(" a ", "error message");
        WebI18nAssert.notBlack("test string", "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testNotBlackWithNullString() {
        WebI18nAssert.notBlack(null, "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testNotBlackWithEmptyString() {
        WebI18nAssert.notBlack("", "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testNotBlackWithBlankString() {
        WebI18nAssert.notBlack("   ", "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testNotBlackWithBlankStringAndParams() {
        WebI18nAssert.notBlack("", "error message with param: %s", "param1");
    }

    @Test
    public void testNotEmptyWithValidCollection() {
        // Should not throw exception
        List<String> list = Arrays.asList("item1", "item2");
        WebI18nAssert.notEmpty(list, "error message");

        Collection<Integer> collection = new ArrayList<>();
        collection.add(1);
        WebI18nAssert.notEmpty(collection, "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testNotEmptyWithNullCollection() {
        WebI18nAssert.notEmpty(null, "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testNotEmptyWithEmptyCollection() {
        WebI18nAssert.notEmpty(Collections.emptyList(), "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testNotEmptyWithEmptyCollectionAndParams() {
        WebI18nAssert.notEmpty(new ArrayList<>(), "error message with param: %s", "param1");
    }

    @Test
    public void testIsTrueWithTrueBoolean() {
        // Should not throw exception
        WebI18nAssert.isTrue(true, "error message");
        WebI18nAssert.isTrue(Boolean.TRUE, "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testIsTrueWithFalseBoolean() {
        WebI18nAssert.isTrue(false, "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testIsTrueWithNullBoolean() {
        WebI18nAssert.isTrue(null, "error message");
    }

    @Test(expected = WebI18nException.class)
    public void testIsTrueWithFalseBooleanAndParams() {
        WebI18nAssert.isTrue(Boolean.FALSE, "error message with param: %s", "param1");
    }

    @Test(expected = WebI18nException.class)
    public void testFail() {
        WebI18nAssert.fail("error message");
    }

    @Test(expected = WebI18nException.class)
    public void testFailWithParams() {
        WebI18nAssert.fail("error message with param: %s", "param1");
    }

    @Test(expected = WebI18nException.class)
    public void testFailWithMultipleParams() {
        WebI18nAssert.fail("error message with params: %s, %s, %d", "param1", "param2", 123);
    }

    @Test
    public void testNotNullWithParams() {
        // Should not throw exception when object is not null
        WebI18nAssert.notNull("test", "error message with param: %s", "param1");
    }

    @Test
    public void testIsNullWithParams() {
        // Should not throw exception when object is null
        WebI18nAssert.isNull(null, "error message with param: %s", "param1");
    }

    @Test
    public void testNotBlackWithParams() {
        // Should not throw exception when string is not blank
        WebI18nAssert.notBlack("test", "error message with param: %s", "param1");
    }

    @Test
    public void testNotEmptyWithParams() {
        // Should not throw exception when collection is not empty
        List<String> list = Collections.singletonList("item");
        WebI18nAssert.notEmpty(list, "error message with param: %s", "param1");
    }

    @Test
    public void testIsTrueWithParams() {
        // Should not throw exception when boolean is true
        WebI18nAssert.isTrue(true, "error message with param: %s", "param1");
    }
}
