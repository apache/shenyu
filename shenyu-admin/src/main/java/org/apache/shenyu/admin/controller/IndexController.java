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

package org.apache.shenyu.admin.controller;

import jakarta.servlet.http.HttpServletRequest;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.Objects;

/**
 * The type Index controller.
 */
@Controller
public class IndexController {
    
    /**
     * Index string.
     *
     * @param model the model
     * @param request the http request
     * @return the string
     */
    @RequestMapping(value = {"/index", "/"})
    public String index(final Model model, final HttpServletRequest request) {
        // Get httpPath from request to ensure it reflects the actual access URL
        // This handles cases where the service is accessed through reverse proxy or load balancer
        String httpPath = buildBaseUrl(request);
        model.addAttribute("domain", httpPath);
        return "index";
    }
    
    /**
     * Build base URL from request.
     * This method constructs the base URL using the actual request information,
     * which is more accurate than server-side configuration when accessed through proxy.
     * Supports reverse proxy scenarios by checking X-Forwarded-Proto and X-Forwarded-Host headers.
     *
     * @param request the http request
     * @return the base URL
     */
    private String buildBaseUrl(final HttpServletRequest request) {
        // Check for reverse proxy headers first
        String scheme = getScheme(request);
        String host = getHost(request);
        String contextPath = request.getContextPath();
        
        StringBuilder url = new StringBuilder();
        url.append(scheme).append("://").append(host);
        if (StringUtils.isNotEmpty(contextPath)) {
            url.append(contextPath);
        }
        
        return url.toString();
    }
    
    /**
     * Get scheme from request, checking X-Forwarded-Proto header for reverse proxy.
     *
     * @param request the http request
     * @return the scheme (http or https)
     */
    private String getScheme(final HttpServletRequest request) {
        String forwardedProto = request.getHeader("X-Forwarded-Proto");
        if (StringUtils.isNotEmpty(forwardedProto)) {
            // Validate the scheme to prevent header injection
            if ("http".equalsIgnoreCase(forwardedProto) || "https".equalsIgnoreCase(forwardedProto)) {
                return forwardedProto.toLowerCase();
            }
        }
        return request.getScheme();
    }
    
    /**
     * Get host from request, checking X-Forwarded-Host header for reverse proxy.
     * Falls back to server name and port if header is not present or invalid.
     * Validates the forwarded host to prevent header injection attacks and open redirect vulnerabilities.
     *
     * @param request the http request
     * @return the host (with port if non-standard)
     */
    private String getHost(final HttpServletRequest request) {
        String forwardedHost = request.getHeader("X-Forwarded-Host");
        if (StringUtils.isNotEmpty(forwardedHost)) {
            // Validate the forwarded host to prevent injection attacks
            String validatedHost = validateForwardedHost(forwardedHost);
            if (Objects.nonNull(validatedHost)) {
                return validatedHost;
            }
            // If validation fails, fall through to use server name
        }
        
        String serverName = request.getServerName();
        int serverPort = request.getServerPort();
        String scheme = getScheme(request);
        
        // Check if port is standard (80 for http, 443 for https)
        boolean isStandardPort = ("http".equals(scheme) && serverPort == 80) || ("https".equals(scheme) && serverPort == 443);
        
        if (isStandardPort) {
            return serverName;
        } else {
            return serverName + ":" + serverPort;
        }
    }

    /**
     * Validate X-Forwarded-Host header to prevent header injection and open redirect attacks.
     * Only allows valid hostname format with optional port number.
     *
     * @param forwardedHost the forwarded host header value
     * @return validated host if valid, null otherwise
     */
    private String validateForwardedHost(final String forwardedHost) {
        if (StringUtils.isEmpty(forwardedHost)) {
            return null;
        }
        
        // Remove whitespace
        String trimmed = forwardedHost.trim();
        
        // Check for empty after trim
        if (trimmed.isEmpty()) {
            return null;
        }
        
        // Check for control characters, protocol separators, or path separators
        // This prevents injection of malicious content like "evil.com/path" or "http://evil.com"
        if (trimmed.contains("://") || trimmed.contains("/") || trimmed.contains("\\") 
                || trimmed.contains("?") || trimmed.contains("#") || trimmed.contains(" ")) {
            return null;
        }
        
        // Check for control characters
        for (int i = 0; i < trimmed.length(); i++) {
            char c = trimmed.charAt(i);
            if (Character.isISOControl(c)) {
                return null;
            }
        }
        
        // Validate hostname format: hostname[:port]
        // Hostname can contain: letters, numbers, dots, hyphens
        // Port must be numeric if present
        String[] parts = trimmed.split(":", 2);
        String hostname = parts[0];
        
        // Validate hostname part
        if (hostname.isEmpty() || hostname.length() > 253) {
            return null;
        }
        
        // Check hostname contains only valid characters
        if (!hostname.matches("^[a-zA-Z0-9.-]+$")) {
            return null;
        }
        
        // Hostname cannot start or end with dot or hyphen
        if (hostname.startsWith(".") || hostname.endsWith(".") 
                || hostname.startsWith("-") || hostname.endsWith("-")) {
            return null;
        }
        
        // Validate port if present
        if (parts.length == 2) {
            String portStr = parts[1];
            if (portStr.isEmpty()) {
                return null;
            }
            try {
                int port = Integer.parseInt(portStr);
                if (port < 1 || port > 65535) {
                    return null;
                }
            } catch (NumberFormatException e) {
                return null;
            }
        }
        
        return trimmed;
    }
}
