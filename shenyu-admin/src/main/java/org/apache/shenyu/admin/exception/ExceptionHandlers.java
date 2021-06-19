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
import org.springframework.validation.BindingResult;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * ControllerMethodResolver.
 *
 * @see <a href="https://dzone.com/articles/global-exception-handling-with-controlleradvice">global-exception-handling-with-controlleradvice</a>
 */
@Slf4j
@ResponseBody
@ControllerAdvice
public class ExceptionHandlers {

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

    @ExceptionHandler(DuplicateKeyException.class)
    protected ShenyuAdminResult serverExceptionHandler(final DuplicateKeyException exception) {
        log.error(exception.getMessage(), exception);
        return ShenyuAdminResult.error(ShenyuResultMessage.UNIQUE_INDEX_CONFLICT_ERROR);
    }

    @ExceptionHandler(UnauthorizedException.class)
    protected ShenyuAdminResult shiroExceptionHandler(final UnauthorizedException exception) {
        log.error(exception.getMessage(), exception);
        return ShenyuAdminResult.error(CommonErrorCode.TOKEN_NO_PERMISSION, ShenyuResultMessage.TOKEN_HAS_NO_PERMISSION);
    }

    @ExceptionHandler(NullPointerException.class)
    protected ShenyuAdminResult nullPointExceptionHandler(final NullPointerException exception) {
        log.error(exception.getMessage(), exception);
        return ShenyuAdminResult.error(CommonErrorCode.NOT_FOUND_EXCEPTION, ShenyuResultMessage.NOT_FOUND_EXCEPTION);
    }

    @ExceptionHandler(HttpRequestMethodNotSupportedException.class)
    protected ShenyuAdminResult handleHttpRequestMethodNotSupportedException(final HttpRequestMethodNotSupportedException e) {
        log.warn("", e);
        StringBuilder sb = new StringBuilder();
        sb.append(e.getMethod());
        sb.append(
                " method is not supported for this request. Supported methods are ");
        e.getSupportedHttpMethods().forEach(t -> sb.append(t + " "));
        return ShenyuAdminResult.error(sb.toString());
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    protected ShenyuAdminResult handleMethodArgumentNotValidException(final MethodArgumentNotValidException e) {
        log.warn("", e);
        BindingResult bindingResult = e.getBindingResult();
        String errorMsg = bindingResult.getFieldErrors().stream()
                .map(f -> f.getField().concat(": ").concat(Optional.ofNullable(f.getDefaultMessage()).orElse("")))
                .collect(Collectors.joining("| "));
        return ShenyuAdminResult.error(String.format("Request error! invalid argument [%s]", errorMsg));
    }

    @ExceptionHandler(MissingServletRequestParameterException.class)
    protected ShenyuAdminResult handleMissingServletRequestParameterException(final MissingServletRequestParameterException e) {
        log.warn("", e);
        return ShenyuAdminResult.error(String.format("%s parameter is missing", e.getParameterName()));
    }

    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    protected ShenyuAdminResult handleMethodArgumentTypeMismatchException(final MethodArgumentTypeMismatchException e) {
        log.warn("", e);
        return ShenyuAdminResult.error(String.format("%s should be of type %s", e.getName(), e.getRequiredType().getName()));
    }

    @ExceptionHandler(ConstraintViolationException.class)
    protected ShenyuAdminResult handleConstraintViolationException(final ConstraintViolationException e) {
        log.warn("", e);
        Set<ConstraintViolation<?>> violations = e.getConstraintViolations();
        return ShenyuAdminResult.error(violations.stream()
                .map(v -> v.getPropertyPath().toString().concat(": ").concat(v.getMessage()))
                .collect(Collectors.joining("| ")));
    }
}
