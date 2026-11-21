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

package org.apache.shenyu.admin.security.password;

import jakarta.servlet.http.HttpServletRequest;
import org.apache.shenyu.admin.controller.DashboardUserController;
import org.apache.shenyu.admin.controller.PlatformController;
import org.apache.shenyu.admin.model.dto.DashboardUserModifyPasswordDTO;
import org.apache.shenyu.admin.model.dto.LoginDashboardUserDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.springframework.core.MethodParameter;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpInputMessage;
import org.springframework.http.MediaType;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.http.server.ServletServerHttpRequest;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.servlet.HandlerMapping;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.mvc.method.annotation.RequestBodyAdviceAdapter;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Adapts the two existing password DTOs before conversion and validation, leaving their controllers and services intact.
 */
@Order(Ordered.HIGHEST_PRECEDENCE)
@RestControllerAdvice(assignableTypes = {PlatformController.class, DashboardUserController.class})
public class PasswordEncryptionAdvice extends RequestBodyAdviceAdapter implements ResponseBodyAdvice<Object> {

    private final PasswordEncryptionService service;

    /**
     * Create the narrow transport adapter.
     * @param service envelope service
     */
    public PasswordEncryptionAdvice(final PasswordEncryptionService service) {
        this.service = service;
    }

    @Override
    public boolean supports(final MethodParameter parameter, final Type targetType, final Class<? extends HttpMessageConverter<?>> converterType) {
        return Objects.nonNull(parameter.getMethod()) && ((PlatformController.class.isAssignableFrom(parameter.getContainingClass())
                && "loginDashboardUser".equals(parameter.getMethod().getName()) && targetType == LoginDashboardUserDTO.class)
                || (DashboardUserController.class.isAssignableFrom(parameter.getContainingClass())
                && "modifyPassword".equals(parameter.getMethod().getName()) && targetType == DashboardUserModifyPasswordDTO.class));
    }

    @Override
    public boolean supports(final MethodParameter returnType, final Class<? extends HttpMessageConverter<?>> converterType) {
        return PlatformController.class.isAssignableFrom(returnType.getContainingClass()) && Objects.nonNull(returnType.getMethod())
                && "info".equals(returnType.getMethod().getName());
    }

    @Override
    public HttpInputMessage beforeBodyRead(final HttpInputMessage input, final MethodParameter parameter,
                                           final Type targetType, final Class<? extends HttpMessageConverter<?>> converterType) {
        try {
            if (input.getHeaders().getContentLength() > PasswordEncryptionService.MAX_BODY_BYTES) {
                throw new InvalidPasswordRequestException();
            }
            byte[] encoded = input.getBody().readNBytes(PasswordEncryptionService.MAX_BODY_BYTES + 1);
            String purpose = targetType == LoginDashboardUserDTO.class ? "login" : "change-password";
            String target = "login".equals(purpose) ? "" : pathId(input);
            byte[] decoded = service.decodeRequest(encoded, purpose, target);
            HttpHeaders headers = new HttpHeaders();
            headers.putAll(input.getHeaders());
            headers.remove(HttpHeaders.TRANSFER_ENCODING);
            headers.setContentLength(decoded.length);
            headers.setContentType(MediaType.APPLICATION_JSON);
            return new HttpInputMessage() {
                @Override
                public InputStream getBody() {
                    return new ByteArrayInputStream(decoded);
                }

                @Override
                public HttpHeaders getHeaders() {
                    return headers;
                }
            };
        } catch (IOException exception) {
            throw new InvalidPasswordRequestException();
        }
    }

    @Override
    public Object beforeBodyWrite(final Object body, final MethodParameter returnType, final MediaType selectedContentType,
                                   final Class<? extends HttpMessageConverter<?>> selectedConverterType,
                                   final ServerHttpRequest request, final ServerHttpResponse response) {
        response.getHeaders().setCacheControl("no-store");
        if (body instanceof ShenyuAdminResult) {
            ShenyuAdminResult result = (ShenyuAdminResult) body;
            if (Integer.valueOf(CommonErrorCode.SUCCESSFUL).equals(result.getCode())) {
                result.setData(service.secretInfo());
            }
        }
        return body;
    }

    /**
     * Password validation exceptions contain rejected values; deliberately do not log or render them.
     * @return the established Admin error format without submitted values
     */
    @ExceptionHandler(InvalidPasswordRequestException.class)
    public ShenyuAdminResult invalidRequest() {
        return ShenyuAdminResult.error("Invalid password request");
    }

    /**
     * Keep unrelated DTO field messages compatible without logging rejected values or exception objects.
     * @param exception validation metadata
     * @return generic password failure, or the established non-password field message format
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ShenyuAdminResult invalidArguments(final MethodArgumentNotValidException exception) {
        if (isCredentialType(exception.getParameter().getParameterType())) {
            return invalidRequest();
        }
        String message = exception.getBindingResult().getFieldErrors().stream()
                .map(field -> field.getField().concat(": ").concat(Optional.ofNullable(field.getDefaultMessage()).orElse("")))
                .collect(Collectors.joining("| "));
        return ShenyuAdminResult.error(String.format("Request error! invalid argument [%s]", message));
    }

    /**
     * Keep parse failures value-free, using handler metadata instead of inspecting the exception text or submitted body.
     * @param request mapped servlet request
     * @return generic failure in the established Admin response shape
     */
    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ShenyuAdminResult unreadableRequest(final HttpServletRequest request) {
        Object handler = request.getAttribute(HandlerMapping.BEST_MATCHING_HANDLER_ATTRIBUTE);
        if (handler instanceof HandlerMethod) {
            if (Arrays.stream(((HandlerMethod) handler).getMethod().getParameterTypes()).anyMatch(this::isCredentialType)) {
                return invalidRequest();
            }
            return ShenyuAdminResult.error("The system is busy, please try again later");
        }
        return ShenyuAdminResult.error("Invalid request");
    }

    private boolean isCredentialType(final Class<?> type) {
        return type == LoginDashboardUserDTO.class || type == DashboardUserModifyPasswordDTO.class;
    }

    private String pathId(final HttpInputMessage input) {
        Object variables = null;
        if (input instanceof ServletServerHttpRequest) {
            variables = ((ServletServerHttpRequest) input).getServletRequest().getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
        } else {
            RequestAttributes attributes = RequestContextHolder.getRequestAttributes();
            if (attributes instanceof ServletRequestAttributes) {
                variables = ((ServletRequestAttributes) attributes).getRequest().getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
            }
        }
        if (variables instanceof Map) {
            Object id = ((Map<?, ?>) variables).get("id");
            if (id instanceof String && !((String) id).isEmpty()) {
                return (String) id;
            }
        }
        throw new InvalidPasswordRequestException();
    }
}
