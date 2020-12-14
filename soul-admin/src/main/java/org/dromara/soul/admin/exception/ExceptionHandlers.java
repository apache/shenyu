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

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.common.exception.SoulException;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * ControllerMethodResolver.
 * https://dzone.com/articles/global-exception-handling-with-controlleradvice
 *
 * @author xiaoyu
 */
@Slf4j
@ControllerAdvice
public class ExceptionHandlers {

    @ExceptionHandler(Exception.class)
    @ResponseBody
    protected SoulAdminResult serverExceptionHandler(final Exception exception) {
        log.error(exception.getMessage(), exception);
        String message;
        if (exception instanceof SoulException) {
            SoulException soulException = (SoulException) exception;
            message = soulException.getMessage();
        } else {
            message = "The system is busy, please try again later";
        }
        return SoulAdminResult.error(message);
    }

    @ResponseBody
    @ExceptionHandler(DuplicateKeyException.class)
    protected SoulAdminResult serverExceptionHandler(final DuplicateKeyException exception) {
        log.error(exception.getMessage(), exception);
        return SoulAdminResult.error(SoulResultMessage.UNIQUE_INDEX_CONFLICT_ERROR);
    }
}
