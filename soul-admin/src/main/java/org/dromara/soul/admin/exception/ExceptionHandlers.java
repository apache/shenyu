/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.exception;

import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.common.exception.SoulException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * ControllerMethodResolver.
 * https://dzone.com/articles/global-exception-handling-with-controlleradvice
 *
 * @author xiaoyu
 */
@ControllerAdvice
public class ExceptionHandlers {

    private static final Logger LOGGER = LoggerFactory.getLogger(ExceptionHandlers.class);

    @ExceptionHandler(Exception.class)
    @ResponseBody
    protected SoulAdminResult serverExceptionHandler(final Exception exception) {
        LOGGER.error(exception.getMessage(), exception);
        String message;
        if (exception instanceof SoulException) {
            SoulException soulException = (SoulException) exception;
            message = soulException.getMessage();
        } else {
            message = "系统繁忙,请稍后重试";
        }
        return SoulAdminResult.error(message);
    }
}
