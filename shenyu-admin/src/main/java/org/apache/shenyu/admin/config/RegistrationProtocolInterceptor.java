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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.config.properties.RegistrationFencingProperties;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;
import org.apache.shenyu.register.common.protocol.RegistrationProtocol;
import org.springframework.web.servlet.HandlerInterceptor;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Negotiate registration versions without changing the v1 response body or authentication.
 */
public final class RegistrationProtocolInterceptor implements HandlerInterceptor {

    private final RegistrationFencingProperties fencingProperties;

    /**
     * Construct the legacy-compatible interceptor for isolated callers.
     */
    public RegistrationProtocolInterceptor() {
        this(null);
    }

    /**
     * Construct an interceptor with capability advertisement settings.
     *
     * @param fencingProperties fencing properties
     */
    public RegistrationProtocolInterceptor(final RegistrationFencingProperties fencingProperties) {
        this.fencingProperties = fencingProperties;
    }

    @Override
    public boolean preHandle(final HttpServletRequest request, final HttpServletResponse response, final Object handler) throws IOException {
        if (!"POST".equals(request.getMethod())) {
            return true;
        }
        response.setHeader(RegistrationProtocol.VERSION_HEADER, RegistrationProtocol.V1);
        String capabilities = RegistrationProtocol.V1_CAPABILITIES;
        if (Objects.nonNull(fencingProperties) && fencingProperties.isEnabled()) {
            capabilities = capabilities + "," + RegistrationFencingProtocol.CAPABILITY;
        }
        response.setHeader(RegistrationProtocol.CAPABILITIES_HEADER, capabilities);
        List<String> versions = Collections.list(request.getHeaders(RegistrationProtocol.VERSION_HEADER));
        try {
            if (versions.size() > 1) {
                throw new IllegalArgumentException(RegistrationProtocol.UNSUPPORTED_VERSION);
            }
            RegistrationProtocol.resolveVersion(versions.isEmpty() ? null : versions.get(0));
        } catch (IllegalArgumentException ex) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.setContentType("text/plain;charset=UTF-8");
            response.getWriter().write(RegistrationProtocol.UNSUPPORTED_VERSION);
            return false;
        }
        return true;
    }
}
