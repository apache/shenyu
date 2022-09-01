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

package org.apache.shenyu.admin.aspect;

import org.apache.shenyu.admin.config.properties.DashboardProperties;
import org.apache.shenyu.common.exception.ShenyuException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.Signature;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link PrintApiLogAspect}.
 */
public class PrintApiLogAspectTest {

    private PrintApiLogAspect printApiLogAspect;

    private DashboardProperties dashboardProperties;

    @BeforeEach
    public void setUp() {
        dashboardProperties = mock(DashboardProperties.class);
        printApiLogAspect = new PrintApiLogAspect(dashboardProperties);
    }

    @Test
    public void testAround() throws ShenyuException {
        ProceedingJoinPoint point = mock(ProceedingJoinPoint.class);
        assertDoesNotThrow(() -> printApiLogAspect.logAround(point));
        when(dashboardProperties.getEnablePrintApiLog()).thenReturn(true);
        when(point.getTarget()).thenReturn(mock(PrintApiLogAspectTest.class));
        when(point.getSignature()).thenReturn(mock(Signature.class));
        assertDoesNotThrow(() -> printApiLogAspect.logAround(point));
        try {
            doThrow(ShenyuException.class).when(point).proceed();
            assertThrows(ShenyuException.class, () -> printApiLogAspect.logAround(point));
        } catch (Throwable e) {
            throw new ShenyuException(e);
        }
    }

    @Test
    public void pointCutTest() {
        assertDoesNotThrow(() -> printApiLogAspect.pointCut());
    }
}
