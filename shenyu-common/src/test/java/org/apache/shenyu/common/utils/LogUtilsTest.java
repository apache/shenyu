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

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;

import java.util.function.Supplier;

import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link LogUtils}.
 */
public final class LogUtilsTest {

    private Logger logger;

    private Supplier<Object> supplier;

    @Before
    public void setUp() {
        logger = spy(Logger.class);
        supplier = mock(Supplier.class);
        when(supplier.get()).thenReturn("Test case for LogUtils");
    }

    @Test
    public void testAtLeastOnceDebugWithFormat() {
        when(logger.isDebugEnabled()).thenReturn(true);
        LogUtils.debug(logger, "testDebug: {}", supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testAtLeastOnceDebugWithFormatBySimple() {
        LogUtils.debug("testDebug: {}", supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testAtLeastOnceDebugWithFormatByMultipleParams() {
        String param1 = "param1";
        String param2 = "param2";
        String param3 = "param3";
        String param4 = "param4";
        String param5 = "param5";
        LogUtils.debug("testDebug: {},{},{},{},{}", param1, param2, param3, param4, param5);
    }

    @Test
    public void testNeverDebugWithFormat() {
        when(logger.isDebugEnabled()).thenReturn(false);
        LogUtils.debug(logger, "testDebug: {}", supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testNeverDebugWithFormatBySimple() {
        LogUtils.debug("testDebug: {}", supplier);
    }

    @Test
    public void testAtLeastOnceDebug() {
        when(logger.isDebugEnabled()).thenReturn(true);
        LogUtils.debug(logger, supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testAtLeastOnceDebugBySimple() {
        LogUtils.debug(supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverDebug() {
        when(logger.isDebugEnabled()).thenReturn(false);
        LogUtils.debug(logger, supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testNeverDebugBySimple() {
        LogUtils.debug(supplier);
    }

    @Test
    public void testAtLeastOnceInfoWithFormat() {
        when(logger.isInfoEnabled()).thenReturn(true);
        LogUtils.info(logger, "testInfo: {}", supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testAtLeastOnceInfoWithFormatByMultipleParams() {
        String param1 = "param1";
        String param2 = "param2";
        String param3 = "param3";
        String param4 = "param4";
        String param5 = "param5";
        LogUtils.info("testInfo: {},{},{},{},{}", param1, param2, param3, param4, param5);
    }

    @Test
    public void testAtLeastOnceInfoWithFormatBySimple() {
        LogUtils.info("testInfo: {}", supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverInfoWithFormat() {
        when(logger.isInfoEnabled()).thenReturn(false);
        LogUtils.info(logger, "testInfo: {}", supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testNeverInfoWithFormatBySimple() {
        LogUtils.info("testInfo: {}", supplier);
    }

    @Test
    public void testAtLeastOnceInfo() {
        when(logger.isInfoEnabled()).thenReturn(true);
        LogUtils.info(logger, supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testAtLeastOnceInfoBySimple() {
        LogUtils.info(supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverInfo() {
        when(logger.isInfoEnabled()).thenReturn(false);
        LogUtils.info(logger, supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testNeverInfoBySimple() {
        LogUtils.info(supplier);
    }

    @Test
    public void testAtLeastOnceErrorFormat() {
        when(logger.isErrorEnabled()).thenReturn(true);
        LogUtils.error(logger, "testError: {}", supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testAtLeastOnceErrorFormatBySimple() {
        LogUtils.error("testError: {}", supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverErrorFormat() {
        when(logger.isErrorEnabled()).thenReturn(false);
        LogUtils.error(logger, "testError: {}", supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testNeverErrorFormatByMultipleParams() {
        String param1 = "param1";
        String param2 = "param2";
        String param3 = "param3";
        String param4 = "param4";
        String param5 = "param5";
        LogUtils.error("testError: {},{},{},{},{}", param1, param2, param3, param4, param5);
    }

    @Test
    public void testNeverErrorFormatBySimple() {
        LogUtils.error("testError: {}", supplier);
    }

    @Test
    public void testAtLeastOnceError() {
        when(logger.isErrorEnabled()).thenReturn(true);
        LogUtils.error(logger, supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testAtLeastOnceErrorBySimple() {
        LogUtils.error(supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverError() {
        when(logger.isErrorEnabled()).thenReturn(false);
        LogUtils.error(logger, supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testNeverErrorBySimple() {
        LogUtils.error(supplier);
    }

    @Test
    public void testAtLeastOnceWarnWithFormat() {
        when(logger.isWarnEnabled()).thenReturn(true);
        LogUtils.warn(logger, "testWarn: {}", supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testAtLeastOnceWarnWithFormatByMultipleParams() {
        String param1 = "param1";
        String param2 = "param2";
        String param3 = "param3";
        String param4 = "param4";
        String param5 = "param5";
        LogUtils.warn("testWarn: {},{},{},{},{}", param1, param2, param3, param4, param5);
    }

    @Test
    public void testAtLeastOnceWarnWithFormatBySimple() {
        LogUtils.warn("testWarn: {}", supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverWarnWithFormat() {
        when(logger.isWarnEnabled()).thenReturn(false);
        LogUtils.warn(logger, "testWarn: {}", supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testNeverWarnWithFormatBySimple() {
        LogUtils.warn("testWarn: {}", supplier);
    }

    @Test
    public void testAtLeastOnceWarn() {
        when(logger.isWarnEnabled()).thenReturn(true);
        LogUtils.warn(logger, supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testAtLeastOnceWarnBySimple() {
        LogUtils.warn(supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverWarn() {
        when(logger.isWarnEnabled()).thenReturn(false);
        LogUtils.warn(logger, supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testNeverWarnBySimple() {
        LogUtils.warn(supplier);
    }
}
