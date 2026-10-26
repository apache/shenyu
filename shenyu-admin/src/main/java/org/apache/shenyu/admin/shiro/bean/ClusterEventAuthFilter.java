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

import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.shiro.web.filter.AccessControlFilter;
import org.apache.shenyu.admin.config.properties.ClusterProperties;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;

/**
 * Dedicated authentication for the internal configuration event endpoint.
 */
public final class ClusterEventAuthFilter extends AccessControlFilter {

    public static final String HEADER = "X-Shenyu-Cluster-Event-Secret";

    private final ClusterProperties properties;

    public ClusterEventAuthFilter(final ClusterProperties properties) {
        this.properties = properties;
    }

    @Override
    protected boolean isAccessAllowed(final ServletRequest request, final ServletResponse response, final Object mappedValue) {
        String expected = properties.getEventSecret();
        String supplied = ((HttpServletRequest) request).getHeader(HEADER);
        return properties.isEnabled() && StringUtils.isNotBlank(expected) && StringUtils.isNotBlank(supplied)
                && MessageDigest.isEqual(expected.getBytes(StandardCharsets.UTF_8), supplied.getBytes(StandardCharsets.UTF_8));
    }

    @Override
    protected boolean onAccessDenied(final ServletRequest request, final ServletResponse response) {
        ((HttpServletResponse) response).setStatus(HttpServletResponse.SC_FORBIDDEN);
        return false;
    }
}
