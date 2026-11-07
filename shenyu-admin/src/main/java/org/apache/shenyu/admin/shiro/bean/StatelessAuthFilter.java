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

package org.apache.shenyu.admin.shiro.bean;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.security.SecurityAuditEventType;
import org.apache.shenyu.admin.security.SecurityAuditLog;
import org.apache.shenyu.admin.utils.SecurityAuditUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.BearerToken;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.web.filter.AccessControlFilter;
import org.springframework.http.HttpMethod;

import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Arrays;
import java.util.Objects;

/**
 * custom Stateless AccessControlFilter.
 */
public class StatelessAuthFilter extends AccessControlFilter {

    @Override
    protected boolean isAccessAllowed(final ServletRequest servletRequest,
                                      final ServletResponse servletResponse,
                                      final Object o) {
        return false;
    }
    
    @Override
    protected boolean onAccessDenied(final ServletRequest servletRequest,
                                     final ServletResponse servletResponse) throws Exception {
        HttpServletRequest httpServletRequest = (HttpServletRequest) servletRequest;
        if (StringUtils.equals(HttpMethod.OPTIONS.name(), httpServletRequest.getMethod())) {
            return true;
        }
        
        String tokenValue = getTokenValue(httpServletRequest);
        if (StringUtils.isBlank(tokenValue)) {
            SecurityAuditLog.event(SecurityAuditEventType.REQUEST_AUTH)
                    .outcome(SecurityAuditLog.Outcome.FAILURE)
                    .action("request_auth")
                    .reason("missing_token")
                    .resource(httpServletRequest.getRequestURI())
                    .remoteAddr(httpServletRequest.getRemoteAddr())
                    .emit();
            unionFailResponse(servletResponse);
            return false;
        }

        BearerToken token = new BearerToken(tokenValue);

        Subject subject = getSubject(servletRequest, servletResponse);

        try {
            subject.login(token);
        } catch (Exception e) {
            SecurityAuditLog.event(SecurityAuditEventType.REQUEST_AUTH)
                    .outcome(SecurityAuditLog.Outcome.FAILURE)
                    .action("request_auth")
                    .reason(classifyAuthenticationFailure(e))
                    .resource(httpServletRequest.getRequestURI())
                    .remoteAddr(httpServletRequest.getRemoteAddr())
                    .fingerprint(SecurityAuditUtils.fingerprint(tokenValue))
                    .emit();
            unionFailResponse(servletResponse);
            return false;
        }

        return true;
    }

    /**
     * classify the authentication failure into a fixed safe reason, without logging the raw exception
     * message which may carry untrusted token content.
     *
     * @param exception login exception
     * @return fixed reason category
     */
    private static String classifyAuthenticationFailure(final Exception exception) {
        if (exception instanceof AuthenticationException) {
            return "authentication_failed";
        }
        return "authentication_error";
    }
    
    /**
     * get token value from request.
     *
     * @param httpServletRequest request
     * @return tokenValue
     */
    private String getTokenValue(final HttpServletRequest httpServletRequest) {
        String tokenName = Constants.X_ACCESS_TOKEN;
        // get token from header
        String tokenValue = httpServletRequest.getHeader(tokenName);
        if (StringUtils.isNoneBlank(tokenValue)) {
            return tokenValue;
        }
        tokenValue = httpServletRequest.getParameter(tokenName);
        if (StringUtils.isNoneBlank(tokenValue)) {
            return tokenValue;
        }
        Cookie[] cookies = httpServletRequest.getCookies();
        if (Objects.isNull(cookies)) {
            return "";
        }
        return Arrays.stream(cookies)
                .filter(cookie -> StringUtils.equals(tokenName, cookie.getName()))
                .findAny()
                .orElse(new Cookie(tokenName, ""))
                .getValue();
    }
    
    /**
     * union response same result form exception.
     *
     * @param response {@link ServletResponse}
     */
    private void unionFailResponse(final ServletResponse response) throws IOException {
        HttpServletResponse httpResponse = (HttpServletResponse) response;
        httpResponse.setContentType("application/json;charset=utf-8");
        httpResponse.setCharacterEncoding("utf-8");
        wrapCorsResponse(httpResponse);
        httpResponse.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        ShenyuAdminResult result = ShenyuAdminResult.error(CommonErrorCode.TOKEN_ERROR,
                ShenyuResultMessage.TOKEN_IS_ERROR);
        httpResponse.getWriter().println(GsonUtils.getInstance().toJson(result));
    }
    
    /**
     * add cors.
     *
     * @param response {@link ServletResponse}
     */
    private void wrapCorsResponse(final HttpServletResponse response) {
        response.addHeader("Access-Control-Allow-Origin", "*");
        response.addHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE");
        response.addHeader("Access-Control-Allow-Headers", "Content-Type");
        response.addHeader("Access-Control-Max-Age", "1800");
    }
}
