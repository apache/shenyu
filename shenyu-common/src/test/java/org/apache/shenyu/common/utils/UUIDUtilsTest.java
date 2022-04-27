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

package org.apache.shenyu.common.utils;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for UUIDUtils.
 */
public final class UUIDUtilsTest {

    @Test
    public void testGetInstance() {
        UUIDUtils uuidUtils = UUIDUtils.getInstance();
        assertNotNull(uuidUtils);
    }

    @Test
    public void testGenerateShortUuid() {
        String shortUuid = UUIDUtils.getInstance().generateShortUuid();
        assertTrue(StringUtils.isNotEmpty(shortUuid));
        assertEquals(19, shortUuid.length());
    }

    @Test
    public void testConstructor() throws Exception {
        Class<?> uUIDUtilsClass = UUIDUtils.getInstance().getClass();
        Class<?>[] p = {long.class, long.class, long.class};
        Constructor<?> constructor = uUIDUtilsClass.getDeclaredConstructor(p);
        constructor.setAccessible(true);
        try {
            constructor.newInstance(-1L, 10L, 10L);
        } catch (InvocationTargetException ex) {
            assertTrue(ex.getCause().getMessage().startsWith("worker Id can't be greater than"));
        }

        try {
            constructor.newInstance(10L, -1L, 10L);
        } catch (InvocationTargetException ex) {
            assertTrue(ex.getCause().getMessage().startsWith("datacenter Id can't be greater than"));
        }
    }

    @Test
    public void testTilNextMillis() throws Exception {
        Class<?> uUIDUtilsClass = UUIDUtils.getInstance().getClass();
        Class<?>[] p = {long.class};
        Method method = uUIDUtilsClass.getDeclaredMethod("tilNextMillis", p);
        method.setAccessible(true);
        long lastTimestamp = System.currentTimeMillis();
        long result = (long) method.invoke(UUIDUtils.getInstance(), lastTimestamp);
        assertThat(result, greaterThan(lastTimestamp));
    }

    @Test
    public void testNextIdException() throws Exception {
        UUIDUtils uuidUtils = UUIDUtils.getInstance();
        Class<?> uUIDUtilsClass = uuidUtils.getClass();
        Field field = uUIDUtilsClass.getDeclaredField("lastTimestamp");
        field.setAccessible(true);
        field.set(uuidUtils, 1617757060000L);

        Method method = uUIDUtilsClass.getDeclaredMethod("nextId");
        method.setAccessible(true);
        try {
            method.invoke(UUIDUtils.getInstance());
        } catch (InvocationTargetException ex) {
            assertTrue(ex.getCause().getMessage().startsWith("Clock moved backwards."));
        }
    }

    @Test
    public void testNextId() throws Exception {
        UUIDUtils uuidUtils = UUIDUtils.getInstance();
        Class<?> uUIDUtilsClass = uuidUtils.getClass();
        Field field = uUIDUtilsClass.getDeclaredField("lastTimestamp");
        field.setAccessible(true);
        field.set(uuidUtils, System.currentTimeMillis());

        Method method = uUIDUtilsClass.getDeclaredMethod("nextId");
        method.setAccessible(true);
        method.invoke(UUIDUtils.getInstance());
    }

}
