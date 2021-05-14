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

import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shiro.authz.UnauthorizedException;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * ControllerMethodResolver.
 *
 * @see <a href="https://dzone.com/articles/global-exception-handling-with-controlleradvice">global-exception-handling-with-controlleradvice</a>
 */
@Slf4j
@ControllerAdvice
public class ExceptionHandlers {

    @ResponseBody
    @ExceptionHandler(Exception.class)
    protected ShenyuAdminResult serverExceptionHandler(final Exception exception) {
        log.error(exception.getMessage(), exception);
        String message;
        if (exception instanceof ShenyuException) {
            ShenyuException shenyuException = (ShenyuException) exception;
            message = shenyuException.getMessage();
        } else {
            message = "The system is busy, please try again later";
        }
        return ShenyuAdminResult.error(message);
    }

    @ResponseBody
    @ExceptionHandler(DuplicateKeyException.class)
    protected ShenyuAdminResult serverExceptionHandler(final DuplicateKeyException exception) {
        log.error(exception.getMessage(), exception);
        return ShenyuAdminResult.error(ShenyuResultMessage.UNIQUE_INDEX_CONFLICT_ERROR);
    }

    @ResponseBody
    @ExceptionHandler(UnauthorizedException.class)
    private ShenyuAdminResult shiroExceptionHandler(final UnauthorizedException exception) {
        log.error(exception.getMessage(), exception);
        return ShenyuAdminResult.error(CommonErrorCode.TOKEN_NO_PERMISSION, ShenyuResultMessage.TOKEN_HAS_NO_PERMISSION);
    }

    @ResponseBody
    @ExceptionHandler(NullPointerException.class)
    private ShenyuAdminResult nullPointExceptionHandler(final NullPointerException exception) {
        log.error(exception.getMessage(), exception);
        return ShenyuAdminResult.error(CommonErrorCode.NOT_FOUND_EXCEPTION, ShenyuResultMessage.NOT_FOUND_EXCEPTION);
    }
}
