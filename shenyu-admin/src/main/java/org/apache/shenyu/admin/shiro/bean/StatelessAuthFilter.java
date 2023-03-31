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
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shiro.authc.BearerToken;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.web.filter.AccessControlFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpMethod;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * custom Stateless AccessControlFilter.
 */
public class StatelessAuthFilter extends AccessControlFilter {

    private static final Logger LOG = LoggerFactory.getLogger(StatelessAuthFilter.class);

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

        String tokenValue = httpServletRequest.getHeader(Constants.X_ACCESS_TOKEN);
        if (StringUtils.isBlank(tokenValue)) {
            LOG.error("token is null.");
            unionFailResponse(servletResponse);
            return false;
        }

        BearerToken token = new BearerToken(tokenValue);

        Subject subject = getSubject(servletRequest, servletResponse);

        try {
            subject.login(token);
        } catch (Exception e) {
            LOG.error("token is warning. token : {}.", tokenValue, e);
            unionFailResponse(servletResponse);
            return false;
        }

        return true;
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
