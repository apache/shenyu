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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.slf4j.MDC;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link RequestIdFilter}.
 */
public class RequestIdFilterTest {

    private final RequestIdFilter filter = new RequestIdFilter();

    @AfterEach
    public void tearDown() {
        MDC.clear();
    }

    @Test
    public void testValidHeaderIsUsedAndMdcIsRemovedAfterwards() throws Exception {
        String inside = runAndCapture("req_ABC-123");
        assertThat(inside, is("req_ABC-123"));
        assertThat(MDC.get(SecurityAuditLog.REQUEST_ID_MDC_KEY), is(nullValue()));
    }

    @Test
    public void testMissingHeaderGeneratesUuid() throws Exception {
        assertUuid(runAndCapture(null));
    }

    @Test
    public void testIllegalHeadersAreReplacedWithUuid() throws Exception {
        assertUuid(runAndCapture("bad id with spaces"));
        assertUuid(runAndCapture("x".repeat(65)));
        assertUuid(runAndCapture("evil\r\nforged"));
        assertUuid(runAndCapture("id=with;chars"));
    }

    @Test
    public void testBoundaryLengths() throws Exception {
        assertThat(runAndCapture("a"), is("a"));
        assertThat(runAndCapture("x".repeat(64)), is("x".repeat(64)));
        assertUuid(runAndCapture(""));
    }

    @Test
    public void testMdcIsRemovedWhenChainThrows() {
        HttpServletRequest request = mockRequest("req-1");
        FilterChain throwing = (req, res) -> {
            throw new ServletException("boom");
        };
        assertThrows(ServletException.class, () -> filter.doFilter(request, mock(HttpServletResponse.class), throwing));
        assertThat(MDC.get(SecurityAuditLog.REQUEST_ID_MDC_KEY), is(nullValue()));
    }

    @Test
    public void testRepeatedRequestsOnSameThreadDoNotLeakIds() throws Exception {
        String first = runAndCapture("first-id");
        String second = runAndCapture(null);
        assertThat(first, is("first-id"));
        assertThat(second, not(is("first-id")));
        assertUuid(second);
        assertThat(MDC.get(SecurityAuditLog.REQUEST_ID_MDC_KEY), is(nullValue()));
    }

    /**
     * run the filter with the given X-Request-Id header and capture the MDC request id from
     * inside the chain.
     *
     * @param headerValue header value, null for a missing header
     * @return request id visible inside the filter chain
     */
    private String runAndCapture(final String headerValue) throws Exception {
        AtomicReference<String> inside = new AtomicReference<>();
        filter.doFilter(mockRequest(headerValue), mock(HttpServletResponse.class),
                (req, res) -> inside.set(MDC.get(SecurityAuditLog.REQUEST_ID_MDC_KEY)));
        return inside.get();
    }

    /**
     * build a request mock returning the given X-Request-Id header.
     *
     * @param headerValue header value, null for a missing header
     * @return request mock
     */
    private HttpServletRequest mockRequest(final String headerValue) {
        HttpServletRequest request = mock(HttpServletRequest.class);
        when(request.getHeader(RequestIdFilter.REQUEST_ID_HEADER)).thenReturn(headerValue);
        return request;
    }

    /**
     * assert the value parses as a UUID.
     *
     * @param value request id to check
     */
    private void assertUuid(final String value) {
        assertDoesNotThrow(() -> UUID.fromString(value));
    }
}
