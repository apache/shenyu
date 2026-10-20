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

package org.apache.shenyu.admin.mode.cluster.filter;

import jakarta.annotation.Resource;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.PathMatcher;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.filter.OncePerRequestFilter;
import org.springframework.web.util.UriComponentsBuilder;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;

/**
 * Cluster forward filter.
 */
public class ClusterForwardFilter extends OncePerRequestFilter {
    
    private static final Logger LOG = LoggerFactory.getLogger(ClusterForwardFilter.class);
    
    private static final PathMatcher PATH_MATCHER = new AntPathMatcher();

    private static final Set<String> HOP_BY_HOP_HEADERS = Set.of(
            "connection",
            "keep-alive",
            "proxy-authenticate",
            "proxy-authorization",
            "te",
            "trailer",
            "transfer-encoding",
            "upgrade"
    );
    
    private final RestTemplate restTemplate;
    
    @Resource
    private ClusterSelectMasterService clusterSelectMasterService;
    
    @Resource
    private ClusterProperties clusterProperties;
    
    public ClusterForwardFilter(final RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    public ClusterForwardFilter(final RestTemplate restTemplate,
                                final ClusterSelectMasterService clusterSelectMasterService,
                                final ClusterProperties clusterProperties) {
        this.restTemplate = restTemplate;
        this.clusterSelectMasterService = clusterSelectMasterService;
        this.clusterProperties = clusterProperties;
    }
    
    @Override
    protected void doFilterInternal(@NotNull final HttpServletRequest request,
                                    @NotNull final HttpServletResponse response,
                                    @NotNull final FilterChain filterChain) throws ServletException, IOException {
        String method = request.getMethod();
        
        String uri = request.getRequestURI();
        String requestContextPath = request.getContextPath();
        String simpleUri = uri.replaceAll(requestContextPath, "");
        
        if (StringUtils.equals(HttpMethod.OPTIONS.name(), method)) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("method is OPTIONS or GET, no forward :{}", simpleUri);
            }
            filterChain.doFilter(request, response);
            return;
        }
        
        if (clusterSelectMasterService.isMaster()) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("this node is master, no forward :{}", simpleUri);
            }
            filterChain.doFilter(request, response);
            return;
        }
        // this node is not master
        
        // check whether the uri should be ignored
        boolean shouldIgnore = clusterProperties.getIgnoredList()
                .stream().anyMatch(x -> PATH_MATCHER.match(x, simpleUri));
        if (shouldIgnore) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("shouldIgnore, no forward :{}", simpleUri);
            }
            filterChain.doFilter(request, response);
            return;
        }
        
        // check whether the uri should be forwarded
        boolean shouldForward = clusterProperties.getForwardList()
                .stream().anyMatch(x -> PATH_MATCHER.match(x, simpleUri));
        if (!shouldForward) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("!shouldForward, no forward :{}", simpleUri);
            }
            filterChain.doFilter(request, response);
            return;
        }
        
        // cluster forward request to master
        forwardRequest(request, response);
    }
    
    private void forwardRequest(final HttpServletRequest request,
                                final HttpServletResponse response) throws IOException {
        String targetUrl = getForwardingUrl(request);
        
        if (LOG.isDebugEnabled()) {
            LOG.debug("forwarding current uri: {} method: {} request to target url: {}", request.getRequestURI(), request.getMethod(), targetUrl);
        }
        // Create request entity
        HttpHeaders headers = new HttpHeaders();
        // Copy request headers
        copyHeaders(request, headers);
        HttpEntity<byte[]> requestEntity = new HttpEntity<>(getBody(request), headers);
        // Send request
        ResponseEntity<byte[]> responseEntity = restTemplate.exchange(targetUrl, HttpMethod.valueOf(request.getMethod()), requestEntity, byte[].class);
        
        // Set response status and headers
        int statusCode = responseEntity.getStatusCode().value();
        response.setStatus(statusCode);
        // Copy response headers
        copyHeaders(responseEntity.getHeaders(), response);
        // fix cors error
        if (!response.containsHeader("Access-Control-Allow-Origin")) {
            response.setHeader("Access-Control-Allow-Origin", "*");
        }
        if (!response.containsHeader("Access-Control-Allow-Methods")) {
            response.setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE");
        }
        // write response back
        byte[] body = Objects.nonNull(responseEntity.getBody()) ? responseEntity.getBody() : new byte[0];
        if (statusCode != HttpServletResponse.SC_NO_CONTENT
                && statusCode != HttpServletResponse.SC_NOT_MODIFIED
                && (statusCode < 100 || statusCode >= 200)) {
            response.setContentLength(body.length);
        }
        if (body.length > 0) {
            IOUtils.copy(new ByteArrayInputStream(body), response.getOutputStream());
            response.getOutputStream().flush();
        } else {
            response.flushBuffer();
        }
    }
    
    @NotNull
    private String getForwardingUrl(final HttpServletRequest request) {
        ClusterMasterDTO master = clusterSelectMasterService.getMaster();
        String host = master.getMasterHost();
        String port = master.getMasterPort();
        String masterContextPath = master.getContextPath();
        UriComponentsBuilder builder = UriComponentsBuilder
                .fromUri(URI.create(request.getRequestURI()))
                .scheme(request.getScheme())
                .host(host)
                .port(port)
                .query(request.getQueryString());
        String originalPath = builder.build().getPath();
        
        if (StringUtils.isNotEmpty(originalPath)) {
            // remove current context path
            String currentContextPath = request.getContextPath();
            if (StringUtils.isNotEmpty(currentContextPath) && originalPath.startsWith(currentContextPath)) {
                originalPath = originalPath.substring(originalPath.indexOf(currentContextPath) + currentContextPath.length());
            }
        }
        if (StringUtils.isNoneBlank(masterContextPath)) {
            originalPath = masterContextPath + originalPath;
        }
        builder.replacePath(originalPath);
        
        return builder.toUriString();
    }
    
    void copyHeaders(final HttpServletRequest request, final HttpHeaders headers) {
        Set<String> hopByHopHeaders = buildHopByHopHeaders(request);
        Collections.list(request.getHeaderNames())
                .forEach(headerName -> {
                    String name = removeSpecial(headerName);
                    String lowerName = name.toLowerCase(Locale.ROOT);
                    if (!hopByHopHeaders.contains(lowerName)
                            && !HttpHeaders.HOST.equalsIgnoreCase(lowerName)
                            && !HttpHeaders.CONTENT_LENGTH.equalsIgnoreCase(lowerName)) {
                        Collections.list(request.getHeaders(headerName))
                                .forEach(headerValue -> headers.add(name, removeSpecial(headerValue)));
                    }
                });
    }
    
    void copyHeaders(final HttpHeaders sourceHeaders, final HttpServletResponse response) {
        Set<String> hopByHopHeaders = buildHopByHopHeaders(sourceHeaders);
        sourceHeaders.forEach((headerName, headerValues) -> {
            String name = removeSpecial(headerName);
            String lowerName = name.toLowerCase(Locale.ROOT);
            if (!hopByHopHeaders.contains(lowerName)
                    && !HttpHeaders.CONTENT_LENGTH.equalsIgnoreCase(lowerName)
                    && !response.containsHeader(name)) {
                headerValues.forEach(headerValue -> response.addHeader(name, removeSpecial(headerValue)));
            }
        });
    }

    private Set<String> buildHopByHopHeaders(final HttpServletRequest request) {
        Set<String> hopByHopHeaders = new HashSet<>(HOP_BY_HOP_HEADERS);
        Enumeration<String> connectionHeaders = request.getHeaders(HttpHeaders.CONNECTION);
        if (Objects.nonNull(connectionHeaders)) {
            while (connectionHeaders.hasMoreElements()) {
                String value = connectionHeaders.nextElement();
                if (StringUtils.isNotEmpty(value)) {
                    for (String token : value.split(",")) {
                        hopByHopHeaders.add(token.trim().toLowerCase(Locale.ROOT));
                    }
                }
            }
        }
        return hopByHopHeaders;
    }

    private Set<String> buildHopByHopHeaders(final HttpHeaders sourceHeaders) {
        Set<String> hopByHopHeaders = new HashSet<>(HOP_BY_HOP_HEADERS);
        List<String> connectionValues = sourceHeaders.get(HttpHeaders.CONNECTION);
        if (CollectionUtils.isNotEmpty(connectionValues)) {
            for (String value : connectionValues) {
                if (StringUtils.isNotEmpty(value)) {
                    for (String token : value.split(",")) {
                        hopByHopHeaders.add(token.trim().toLowerCase(Locale.ROOT));
                    }
                }
            }
        }
        return hopByHopHeaders;
    }
    
    private String removeSpecial(final String str) {
        return str.replaceAll("\r", "").replaceAll("\n", "");
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

    public void setClusterSelectMasterService(final ClusterSelectMasterService clusterSelectMasterService) {
        this.clusterSelectMasterService = clusterSelectMasterService;
    }

    public void setClusterProperties(final ClusterProperties clusterProperties) {
        this.clusterProperties = clusterProperties;
    }
    
}
