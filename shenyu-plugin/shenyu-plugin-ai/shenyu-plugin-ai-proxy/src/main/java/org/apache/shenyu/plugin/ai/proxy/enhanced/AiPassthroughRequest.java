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

package org.apache.shenyu.plugin.ai.proxy.enhanced;

import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigValidator;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * HTTP metadata for a single vendor call. The body remains owned by the request publisher.
 */
record AiPassthroughRequest(AiUpstreamRequest upstream, String protocol) {

    private static final Set<String> HOP_HEADERS = Set.of("connection", "keep-alive", "proxy-authenticate", "proxy-authorization",
            "te", "trailer", "transfer-encoding", "upgrade");

    static AiPassthroughRequest from(final ServerHttpRequest client, final AiProxyHandle selector) {
        // 拒绝 CONNECT 请求和带 Upgrade 头的请求。
        if ("CONNECT".equals(client.getMethod().name()) || client.getHeaders().containsKey(HttpHeaders.UPGRADE)) {
            throw new AiProxyException(AiProxyException.Kind.REQUEST, "AI passthrough does not support protocol upgrades or tunnels", null);
        }
        // 从保留原始编码的请求路径中裁剪选择器前缀。
        String path = vendorPath(client.getURI().getRawPath(), selector.getStripPrefix());
        URI base = AiProxyConfigValidator.parseBaseUrl(selector.getBaseUrl());
        // 读取厂商基址的目录，并移除末尾的一个斜杠。
        String directory = base.getRawPath();
        if (directory.endsWith("/")) {
            directory = directory.substring(0, directory.length() - 1);
        }
        String query = client.getURI().getRawQuery();
        AiUpstreamRequest request = new AiUpstreamRequest();
        // 用厂商基址、裁剪后的路径和客户端原始查询串拼出目标 URI。
        request.setUri(URI.create(base.getScheme() + "://" + base.getRawAuthority() + directory + path + (Objects.isNull(query) ? "" : "?" + query)));
        // 将客户端的 HTTP 方法设为厂商请求的方法。
        request.setMethod(client.getMethod().name());
        HttpHeaders headers = new HttpHeaders();
        copyEndToEndHeaders(client.getHeaders(), headers);
        // 移除客户端传入的 Host，出站 Host 由厂商地址决定。
        headers.remove(HttpHeaders.HOST);
        request.setHeaders(headers);
        return new AiPassthroughRequest(request, protocol(path));
    }

    private static String vendorPath(final String rawPath, final String prefix) {
        if (!rawPath.isEmpty() && !rawPath.startsWith("/")) {
            throw new AiProxyException(AiProxyException.Kind.REQUEST, "AI passthrough requires an absolute request path", null);
        }
        if (Objects.isNull(prefix) || prefix.isEmpty()) {
            return rawPath.isEmpty() ? "/" : rawPath;
        }
        // 路径与前缀完全相同时返回根路径，否则只在完整路径段边界裁剪。
        if (rawPath.equals(prefix)) {
            return "/";
        }
        if (!rawPath.startsWith(prefix + "/")) {
            throw new AiProxyException(AiProxyException.Kind.REQUEST, "Request path does not match the AI passthrough prefix", null);
        }
        return rawPath.substring(prefix.length());
    }

    private static String protocol(final String path) {
        // 按厂商路径识别 Chat 或 Responses 协议，其他路径返回 unknown。
        return switch (path) {
            case "/v1/chat/completions" -> OpenAiChat.NAME;
            case "/v1/responses" -> OpenAiResponses.NAME;
            default -> "unknown";
        };
    }

    static void copyEndToEndHeaders(final Map<String, List<String>> source, final HttpHeaders target) {
        // 收集逐跳请求头，再逐项复制其余头及其全部值。
        Set<String> excluded = new HashSet<>(HOP_HEADERS);
        source.forEach((name, values) -> {
            // 将 Connection 列出的头名加入排除集合。
            if (HttpHeaders.CONNECTION.equalsIgnoreCase(name)) {
                values.forEach(value -> {
                    for (String token : value.split(",")) {
                        excluded.add(token.trim().toLowerCase(Locale.ROOT));
                    }
                });
            }
        });
        source.forEach((name, values) -> {
            if (!excluded.contains(name.toLowerCase(Locale.ROOT))) {
                target.put(name, new ArrayList<>(values));
            }
        });
    }
}
