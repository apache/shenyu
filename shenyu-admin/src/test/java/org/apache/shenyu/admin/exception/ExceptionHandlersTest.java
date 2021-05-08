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

package org.apache.shenyu.admin.exception;

import org.apache.shenyu.admin.model.result.SoulAdminResult;
import org.apache.shenyu.admin.utils.SoulResultMessage;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.exception.SoulException;
import org.junit.Before;
import org.junit.Test;
import org.springframework.dao.DuplicateKeyException;

import static org.junit.Assert.assertEquals;

/**
 * Test case for {@link ExceptionHandlers}.
 *
 * @author bigwillc
 */
public final class ExceptionHandlersTest {

    private ExceptionHandlers exceptionHandlersUnderTest;

    @Before
    public void setUp() {
        exceptionHandlersUnderTest = new ExceptionHandlers();
    }

    @Test
    public void testServerExceptionHandlerByException() {
        Exception exception = new Exception();
        SoulAdminResult result = exceptionHandlersUnderTest.serverExceptionHandler(exception);
        assertEquals(result.getCode().intValue(), CommonErrorCode.ERROR);
        assertEquals(result.getMessage(), "The system is busy, please try again later");
    }

    @Test
    public void testServerExceptionHandlerBySoulException() {
        Exception soulException = new SoulException("Test soulException message!");
        SoulAdminResult result = exceptionHandlersUnderTest.serverExceptionHandler(soulException);
        assertEquals(result.getCode().intValue(), CommonErrorCode.ERROR);
        assertEquals(result.getMessage(), soulException.getMessage());
    }

    @Test
    public void testServerExceptionHandlerByDuplicateKeyException() {
        DuplicateKeyException duplicateKeyException = new DuplicateKeyException("Test duplicateKeyException message!");
        SoulAdminResult result = exceptionHandlersUnderTest.serverExceptionHandler(duplicateKeyException);
        assertEquals(result.getCode().intValue(), CommonErrorCode.ERROR);
        assertEquals(result.getMessage(), SoulResultMessage.UNIQUE_INDEX_CONFLICT_ERROR);
    }
}
