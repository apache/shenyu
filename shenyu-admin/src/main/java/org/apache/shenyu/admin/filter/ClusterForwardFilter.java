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

package org.apache.shenyu.admin.filter;

import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.service.ClusterMasterService;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.PathMatcher;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.annotation.Resource;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.util.Collections;
import java.util.Objects;

/**
 * Cluster forward filter.
 */
@Component
public class ClusterForwardFilter extends OncePerRequestFilter {
    
    private static final Logger LOG = LoggerFactory.getLogger(ClusterForwardFilter.class);
    
    private static final PathMatcher PATH_MATCHER = new AntPathMatcher();
    
    @Resource
    private RestTemplate restTemplate;
    
    @Resource
    private ClusterMasterService clusterMasterService;
    
    @Resource
    private ClusterProperties clusterProperties;
    
    @Override
    protected void doFilterInternal(@NotNull final HttpServletRequest request,
                                    @NotNull final HttpServletResponse response,
                                    @NotNull final FilterChain filterChain) throws ServletException, IOException {
        String method = request.getMethod();
        if (StringUtils.equals(HttpMethod.OPTIONS.name(), method)) {
            filterChain.doFilter(request, response);
            return;
        }
        
        if (clusterMasterService.isMaster()) {
            filterChain.doFilter(request, response);
            return;
        }
        
        String uri = request.getRequestURI();
        String contextPath = request.getContextPath();
        String replaced = uri.replaceAll(contextPath, "");
        boolean anyMatch = clusterProperties.getForwardList().stream().anyMatch(x -> PATH_MATCHER.match(x, replaced));
        if (!anyMatch) {
            filterChain.doFilter(request, response);
            return;
        }
        // cluster forward request to master
        forwardRequest(request, response);
    }
    
    private void forwardRequest(final HttpServletRequest request, final HttpServletResponse response) throws IOException {
        ClusterMasterDTO master = clusterMasterService.getMaster();
        String host = master.getMasterHost();
        String port = master.getMasterPort();
        String contextPath = master.getContextPath();
        String url = "http://" + host + ":" + port;
        if (StringUtils.isNotEmpty(master.getContextPath())) {
            url = url + "/" + contextPath;
        }
        url = url + request.getRequestURI();
        LOG.debug("forwarding request to url: {}", url);
        // Create request entity
        HttpHeaders headers = new HttpHeaders();
        copyHeaders(request, headers);
        HttpEntity<byte[]> requestEntity = new HttpEntity<>(getBody(request), headers);
        String urlWithParams = url;
        if (StringUtils.isNotEmpty(request.getQueryString())) {
            urlWithParams += "?" + request.getQueryString();
        }
        // Send request
        ResponseEntity<String> responseEntity = restTemplate.exchange(urlWithParams, HttpMethod.valueOf(request.getMethod()), requestEntity, String.class);
        
        // Set response status and headers
        response.setStatus(responseEntity.getStatusCodeValue());
        copyHeaders(responseEntity.getHeaders(), response);
        // fix cors error
        response.addHeader("Access-Control-Allow-Origin", host);
        response.addHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE");
        
        try (Writer writer = response.getWriter()) {
            writer.write(Objects.requireNonNull(responseEntity.getBody()));
            writer.flush();
        }
    }
    
    private void copyHeaders(final HttpServletRequest request, final HttpHeaders headers) {
        Collections.list(request.getHeaderNames())
                .forEach(headerName -> {
                    headers.add(headerName, sanitizeHeaderValue(request.getHeader(headerName)));
                });
    }
    
    private void copyHeaders(final HttpHeaders sourceHeaders, final HttpServletResponse response) {
        sourceHeaders.forEach((headerName, headerValues) -> {
            if (!response.containsHeader(headerName)) {
                headerValues.forEach(headerValue -> {
                    response.addHeader(headerName, sanitizeHeaderValue(headerValue));
                });
            }
        });
    }
    
    private static String sanitizeHeaderValue(final String headerValue) {
        return headerValue.replace("\r", "").replace("\n", "");
    }
    
    private byte[] getBody(final HttpServletRequest request) throws IOException {
        InputStream is = request.getInputStream();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte[] buffer = new byte[1024];
        int bytesRead;
        while ((bytesRead = is.read(buffer)) != -1) {
            baos.write(buffer, 0, bytesRead);
        }
        return baos.toByteArray();
    }
}
