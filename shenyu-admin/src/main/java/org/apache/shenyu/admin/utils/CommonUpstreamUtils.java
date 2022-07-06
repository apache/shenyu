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

package org.apache.shenyu.admin.utils;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.common.dto.convert.selector.TarsUpstream;
import org.apache.shenyu.common.dto.convert.selector.WebSocketUpstream;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Build upstream for rpc plugin.
 */
public class CommonUpstreamUtils {
    
    /**
     * Build divide upstream divide upstream.
     *
     * @param host the host
     * @param port the port
     * @return the divide upstream
     */
    public static DivideUpstream buildDefaultDivideUpstream(final String host, final Integer port) {
        return DivideUpstream.builder().upstreamHost("localhost").protocol("http://").upstreamUrl(buildUrl(host, port)).weight(50).warmup(10).timestamp(System.currentTimeMillis())
                .status(Objects.nonNull(port) && StringUtils.isNotBlank(host)).build();
    }

    /**
     * Build alive divide upstream.
     *
     * @param upstreamUrl the upstreamUrl
     * @return the divide upstream
     */
    public static DivideUpstream buildDefaultAliveDivideUpstream(final String upstreamUrl) {
        return DivideUpstream.builder().upstreamHost("localhost").protocol("http://").upstreamUrl(upstreamUrl).weight(50).warmup(10).timestamp(System.currentTimeMillis()).build();
    }

    /**
     * Build divide upstream divide upstream.
     *
     * @param protocol the protocol
     * @param host the host
     * @param port the port
     * @return the divide upstream
     */
    public static DivideUpstream buildDivideUpstream(final String protocol, final String host, final Integer port) {
        return DivideUpstream.builder().upstreamHost("localhost").protocol(protocol).upstreamUrl(buildUrl(host, port)).weight(50).warmup(10).timestamp(System.currentTimeMillis())
                .status(Objects.nonNull(port) && StringUtils.isNotBlank(host)).build();
    }

    /**
     * Build alive divide upstream.
     *
     * @param protocol the protocol
     * @param upstreamUrl the upstreamUrl
     * @return the divide upstream
     */
    public static DivideUpstream buildAliveDivideUpstream(final String protocol, final String upstreamUrl) {
        return DivideUpstream.builder().upstreamHost("localhost").protocol(protocol).upstreamUrl(upstreamUrl).weight(50).warmup(10).timestamp(System.currentTimeMillis()).build();
    }

    /**
     * Build websocket upstream divide upstream.
     *
     * @param protocol the protocol
     * @param host the host
     * @param port the port
     * @return the websocket upstream
     */
    public static WebSocketUpstream buildWebSocketUpstream(final String protocol, final String host, final Integer port) {
        return WebSocketUpstream.builder().host("localhost").protocol(protocol).upstreamUrl(buildUrl(host, port)).weight(50).warmup(10).timestamp(System.currentTimeMillis())
                .status(Objects.nonNull(port) && StringUtils.isNotBlank(host)).build();
    }

    /**
     * Build default dubbo upstream dubbo upstream.
     *
     * @param host the host
     * @param port the port
     * @return the dubbo upstream
     */
    public static DubboUpstream buildDefaultDubboUpstream(final String host, final Integer port) {
        return DubboUpstream.builder().upstreamHost("localhost").protocol("dubbo://").upstreamUrl(buildUrl(host, port)).weight(50).warmup(10).timestamp(System.currentTimeMillis())
                .status(Objects.nonNull(port) && StringUtils.isNotBlank(host)).build();
    }

    /**
     * Build alive dubbo upstream.
     *
     * @param upstreamUrl the upstreamUrl
     * @return the dubbo upstream
     */
    public static DubboUpstream buildAliveDubboUpstream(final String upstreamUrl) {
        return DubboUpstream.builder().upstreamHost("localhost").protocol("dubbo://").upstreamUrl(upstreamUrl).weight(50).warmup(10).timestamp(System.currentTimeMillis()).build();
    }

    /**
     * Build default grpc upstream grpc upstream.
     *
     * @param host the host
     * @param port the port
     * @return the grpc upstream
     */
    public static GrpcUpstream buildDefaultGrpcUpstream(final String host, final Integer port) {
        return GrpcUpstream.builder().upstreamUrl(buildUrl(host, port)).weight(50).timestamp(System.currentTimeMillis())
                .status(Objects.nonNull(port) && StringUtils.isNotBlank(host)).build();
    }

    /**
     * Build alive grpc upstream.
     *
     * @param upstreamUrl the upstreamUrl
     * @return the grpc upstream
     */
    public static GrpcUpstream buildAliveGrpcUpstream(final String upstreamUrl) {
        return GrpcUpstream.builder().upstreamUrl(upstreamUrl).weight(50).timestamp(System.currentTimeMillis()).build();
    }

    /**
     * Build default tars upstream tars upstream.
     *
     * @param host the host
     * @param port the port
     * @return the tars upstream
     */
    public static TarsUpstream buildDefaultTarsUpstream(final String host, final Integer port) {
        return TarsUpstream.builder().upstreamUrl(buildUrl(host, port)).weight(50).warmup(10).timestamp(System.currentTimeMillis())
                .status(Objects.nonNull(port) && StringUtils.isNotBlank(host)).build();
    }

    /**
     * Build alive tars upstream tars upstream.
     *
     * @param upstreamUrl the upstreamUrl
     * @return the tars upstream
     */
    public static TarsUpstream buildAliveTarsUpstream(final String upstreamUrl) {
        return TarsUpstream.builder().upstreamUrl(upstreamUrl).weight(50).warmup(10).timestamp(System.currentTimeMillis()).build();
    }

    /**
     * Convert common upstream list list.
     *
     * @param upstreamList the upstream list
     * @return the list
     */
    public static List<CommonUpstream> convertCommonUpstreamList(final List<? extends CommonUpstream> upstreamList) {
        return Optional.ofNullable(upstreamList)
                .orElse(Collections.emptyList())
                .stream()
                .map(upstream -> new CommonUpstream(upstream.getProtocol(), upstream.getUpstreamHost(), upstream.getUpstreamUrl(), upstream.isStatus(), upstream.getTimestamp()))
                .collect(Collectors.toList());
    }
    
    /**
     * Build url string.
     *
     * @param host the host
     * @param port the port
     * @return the string
     */
    public static String buildUrl(final String host, final Integer port) {
        return Optional.of(String.join(":", host, String.valueOf(port))).orElse(null);
    }
}
