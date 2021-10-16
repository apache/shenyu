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
    public void testNeverDebugWithFormat() {
        when(logger.isDebugEnabled()).thenReturn(false);
        LogUtils.debug(logger, "testDebug: {}", supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testAtLeastOnceDebug() {
        when(logger.isDebugEnabled()).thenReturn(true);
        LogUtils.debug(logger, supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverDebug() {
        when(logger.isDebugEnabled()).thenReturn(false);
        LogUtils.debug(logger, supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testAtLeastOnceInfoWithFormat() {
        when(logger.isInfoEnabled()).thenReturn(true);
        LogUtils.info(logger, "testInfo: {}", supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverInfoWithFormat() {
        when(logger.isInfoEnabled()).thenReturn(false);
        LogUtils.info(logger, "testInfo: {}", supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testAtLeastOnceInfo() {
        when(logger.isInfoEnabled()).thenReturn(true);
        LogUtils.info(logger, supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverInfo() {
        when(logger.isInfoEnabled()).thenReturn(false);
        LogUtils.info(logger, supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testAtLeastOnceErrorFormat() {
        when(logger.isErrorEnabled()).thenReturn(true);
        LogUtils.error(logger, "testError: {}", supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverErrorFormat() {
        when(logger.isErrorEnabled()).thenReturn(false);
        LogUtils.error(logger, "testError: {}", supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testAtLeastOnceError() {
        when(logger.isErrorEnabled()).thenReturn(true);
        LogUtils.error(logger, supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverError() {
        when(logger.isErrorEnabled()).thenReturn(false);
        LogUtils.error(logger, supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testAtLeastOnceWarnWithFormat() {
        when(logger.isWarnEnabled()).thenReturn(true);
        LogUtils.warn(logger, "testWarn: {}", supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverWarnWithFormat() {
        when(logger.isWarnEnabled()).thenReturn(false);
        LogUtils.warn(logger, "testWarn: {}", supplier);
        verify(supplier, never()).get();
    }

    @Test
    public void testAtLeastOnceWarn() {
        when(logger.isWarnEnabled()).thenReturn(true);
        LogUtils.warn(logger, supplier);
        verify(supplier, atLeastOnce()).get();
    }

    @Test
    public void testNeverWarn() {
        when(logger.isWarnEnabled()).thenReturn(false);
        LogUtils.warn(logger, supplier);
        verify(supplier, never()).get();
    }
}
