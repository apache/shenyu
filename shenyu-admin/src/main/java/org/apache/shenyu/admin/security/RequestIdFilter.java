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

package org.apache.shenyu.admin.security;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.MDC;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Objects;
import java.util.UUID;
import java.util.regex.Pattern;

/**
 * Assigns a safe request id to every HTTP request and exposes it through the SLF4J MDC under
 * {@link SecurityAuditLog#REQUEST_ID_MDC_KEY}. A client supplied {@code X-Request-Id} header is
 * honored only when it strictly matches {@code ^[A-Za-z0-9_-]{1,64}$}; anything else is replaced
 * with a random UUID so untrusted header values never reach log lines, and a missing id is
 * indistinguishable from a rejected one. The MDC entry is always removed when the request
 * completes so ids never leak across reused threads.
 */
public class RequestIdFilter extends OncePerRequestFilter {

    /**
     * header carrying a client supplied request id.
     */
    public static final String REQUEST_ID_HEADER = "X-Request-Id";

    private static final Pattern VALID_REQUEST_ID = Pattern.compile("^[A-Za-z0-9_-]{1,64}$");

    @Override
    protected void doFilterInternal(final HttpServletRequest request, final HttpServletResponse response, final FilterChain filterChain)
            throws ServletException, IOException {
        MDC.put(SecurityAuditLog.REQUEST_ID_MDC_KEY, resolveRequestId(request));
        try {
            filterChain.doFilter(request, response);
        } finally {
            MDC.remove(SecurityAuditLog.REQUEST_ID_MDC_KEY);
        }
    }

    /**
     * resolve the request id: the client header when it is strictly well-formed, else a random UUID.
     *
     * @param request current request
     * @return safe request id
     */
    private String resolveRequestId(final HttpServletRequest request) {
        final String header = request.getHeader(REQUEST_ID_HEADER);
        if (Objects.nonNull(header) && VALID_REQUEST_ID.matcher(header).matches()) {
            return header;
        }
        return UUID.randomUUID().toString();
    }
}
