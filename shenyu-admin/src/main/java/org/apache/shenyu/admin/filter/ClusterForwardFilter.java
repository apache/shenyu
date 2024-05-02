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

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.service.ClusterMasterService;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.http.server.ServletServerHttpRequest;
import org.springframework.stereotype.Component;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.PathMatcher;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.filter.OncePerRequestFilter;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;
import org.springframework.web.util.UriComponentsBuilder;

import javax.annotation.Resource;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
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
    
    @Autowired
    private RequestMappingHandlerMapping requestMappingHandlerMapping;
    
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
        String requestContextPath = request.getContextPath();
        String replaced = uri.replaceAll(requestContextPath, "");
        boolean anyMatch = clusterProperties.getForwardList().stream().anyMatch(x -> PATH_MATCHER.match(x, replaced));
        if (!anyMatch) {
            filterChain.doFilter(request, response);
            return;
        }
        
        // cluster forward request to master
        forwardRequest(request, response, filterChain);
    }
    
    private void forwardRequest(final HttpServletRequest request,
                                final HttpServletResponse response,
                                final FilterChain filterChain) throws IOException, ServletException {
        ClusterMasterDTO master = clusterMasterService.getMaster();
        String host = master.getMasterHost();
        String port = master.getMasterPort();
        //        String contextPath = master.getContextPath();
        
        // new URL
        //        String uri = request.getRequestURI();
        //        String requestContextPath = request.getContextPath();
        //        uri = uri.replaceAll(requestContextPath, "");
        //        if (StringUtils.isNotEmpty(contextPath)) {
        //            uri = contextPath + "/" + uri;
        //        }
        UriComponentsBuilder builder = UriComponentsBuilder
                .fromHttpRequest(new ServletServerHttpRequest(request))
                .host(host)
                .port(port);
        
        // 添加查询参数
        if (StringUtils.isNotEmpty(request.getQueryString())) {
            builder.query(request.getQueryString());
        }
//        HandlerExecutionChain handlerExecutionChain = null;
//            handlerExecutionChain = requestMappingHandlerMapping.getHandler(request);
//            if (handlerExecutionChain == null) {
//                // 没有匹配的处理程序
//                return filterChain.doFilter(request, response);
//            }
//            // 获取目标方法
//            HandlerMethod handlerMethod = (HandlerMethod) handlerExecutionChain.getHandler();
//            Method method = handlerMethod.getMethod();
        LOG.debug("forwarding request to url: {}", builder.toUriString());
        // Create request entity
        HttpHeaders headers = new HttpHeaders();
        
        copyHeaders(request, headers);
        HttpEntity<byte[]> requestEntity = new HttpEntity<>(getBody(request), headers);
        // Send request
        ResponseEntity<byte[]> responseEntity = restTemplate.exchange(builder.toUriString(), HttpMethod.valueOf(request.getMethod()), requestEntity, byte[].class);

        // Set response status and headers
        response.setStatus(responseEntity.getStatusCodeValue());
        copyHeaders(responseEntity.getHeaders(), response);
        // fix cors error
        response.addHeader("Access-Control-Allow-Origin", "*");
        response.addHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE");

//        try (Writer writer = response.getWriter()) {
//            writer.write(URLEncoder.encode(Objects.requireNonNull(responseEntity.getBody()), "UTF-8"));
//            writer.write();
        IOUtils.copy(new ByteArrayInputStream(Objects.requireNonNull(responseEntity.getBody())), response.getOutputStream());
        response.getOutputStream().flush();
//            writer.flush();
//        }
        
    }
    
    private void copyHeaders(final HttpServletRequest request, final HttpHeaders headers) {
        Collections.list(request.getHeaderNames())
                .forEach(headerName -> headers.add(headerName, request.getHeader(headerName).replaceAll("\r", "").replaceAll("\n", "")));
    }
    
    private void copyHeaders(final HttpHeaders sourceHeaders, final HttpServletResponse response) {
        sourceHeaders.forEach((headerName, headerValues) -> {
            String name = headerName.replaceAll("\r", "").replaceAll("\n", "");
            if (!response.containsHeader(name)) {
                headerValues.forEach(headerValue -> {
                    response.addHeader(name, headerValue.replaceAll("\r", "").replaceAll("\n", ""));
                });
            }
        });
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
