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

package org.dromara.soul.admin.exception;

import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.common.exception.CommonErrorCode;
import org.dromara.soul.common.exception.SoulException;
import org.junit.Test;
import org.springframework.dao.DuplicateKeyException;

import static org.junit.Assert.assertEquals;

/**
 * Test case for ExceptionHandlers.
 *
 * @author bigwillc
 */
public final class ExceptionHandlersTest {

    private final Exception exception = new Exception();

    private final Exception soulException = new SoulException("Test soulException message!");

    private final DuplicateKeyException duplicateKeyException = new DuplicateKeyException("Test duplicateKeyException message!");

    @Test
    public void testServerExceptionHandler() {
        SoulAdminResult exceptionRes = new ExceptionHandlers().serverExceptionHandler(exception);
        assertEquals(exceptionRes.getCode().intValue(), CommonErrorCode.ERROR);
        assertEquals(exceptionRes.getMessage(), "The system is busy, please try again later");

        SoulAdminResult soulExceptionRes = new ExceptionHandlers().serverExceptionHandler(soulException);
        assertEquals(soulExceptionRes.getCode().intValue(), CommonErrorCode.ERROR);
        assertEquals(soulExceptionRes.getMessage(), soulException.getMessage());

        SoulAdminResult duplicateKeyExceptionRes = new ExceptionHandlers().serverExceptionHandler(duplicateKeyException);
        assertEquals(duplicateKeyExceptionRes.getCode().intValue(), CommonErrorCode.ERROR);
        assertEquals(duplicateKeyExceptionRes.getMessage(), SoulResultMessage.UNIQUE_INDEX_CONFLICT_ERROR);
    }
}
