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

import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.common.dto.convert.selector.TarsUpstream;
import org.apache.shenyu.common.dto.convert.selector.WebSocketUpstream;
import org.apache.shenyu.register.common.enums.EventType;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * Test case for {@link CommonUpstreamUtils}.
 */
public final class CommonUpstreamUtilsTest {

    private static final String HOST = "127.0.0.1";

    private static final Integer PORT = 8888;

    @Test
    public void buildDefaultDivideUpstreamWithHostAndPort() {
        DivideUpstream divideUpstream = CommonUpstreamUtils.buildDefaultDivideUpstream(HOST, PORT, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(divideUpstream);
        Assert.assertEquals(HOST + ":" + PORT, divideUpstream.getUpstreamUrl());
        Assert.assertEquals(divideUpstream.getUpstreamHost(), "localhost");
    }

    @Test
    public void buildDefaultDivideUpstreamWithPort() {
        DivideUpstream divideUpstream = CommonUpstreamUtils.buildDefaultAliveDivideUpstream(HOST);
        Assert.assertNotNull(divideUpstream);
        Assert.assertEquals(HOST, divideUpstream.getUpstreamUrl());
        Assert.assertEquals(divideUpstream.getUpstreamHost(), "localhost");
    }

    @Test
    public void buildDivideUpstream() {
        DivideUpstream divideUpstream = CommonUpstreamUtils.buildDivideUpstream("http", HOST, PORT, SYS_DEFAULT_NAMESPACE_ID, EventType.REGISTER);
        Assert.assertNotNull(divideUpstream);
        Assert.assertEquals(HOST + ":" + PORT, divideUpstream.getUpstreamUrl());
        Assert.assertEquals("http", divideUpstream.getProtocol());
    }

    @Test
    public void buildAliveDivideUpstream() {
        DivideUpstream divideUpstream = CommonUpstreamUtils.buildAliveDivideUpstream("http", HOST);
        Assert.assertNotNull(divideUpstream);
        Assert.assertEquals(HOST, divideUpstream.getUpstreamUrl());
        Assert.assertEquals("http", divideUpstream.getProtocol());
    }

    @Test
    public void buildWebSocketUpstream() {
        WebSocketUpstream webSocketUpstream = CommonUpstreamUtils.buildWebSocketUpstream("tcp", HOST, PORT, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(webSocketUpstream);
        Assert.assertEquals(HOST + ":" + PORT, webSocketUpstream.getUpstreamUrl());
        Assert.assertEquals("tcp", webSocketUpstream.getProtocol());
    }

    @Test
    public void buildDefaultDubboUpstream() {
        DubboUpstream dubboUpstream = CommonUpstreamUtils.buildDefaultDubboUpstream(HOST, PORT);
        Assert.assertNotNull(dubboUpstream);
        Assert.assertEquals(HOST + ":" + PORT, dubboUpstream.getUpstreamUrl());
        Assert.assertEquals("dubbo://", dubboUpstream.getProtocol());
    }

    @Test
    public void buildAliveDubboUpstream() {
        DubboUpstream dubboUpstream = CommonUpstreamUtils.buildAliveDubboUpstream(HOST);
        Assert.assertNotNull(dubboUpstream);
        Assert.assertEquals(HOST, dubboUpstream.getUpstreamUrl());
        Assert.assertEquals("dubbo://", dubboUpstream.getProtocol());
    }

    @Test
    public void buildDefaultGrpcUpstream() {
        GrpcUpstream grpcUpstream = CommonUpstreamUtils.buildDefaultGrpcUpstream(HOST, PORT, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(grpcUpstream);
        Assert.assertEquals(HOST + ":" + PORT, grpcUpstream.getUpstreamUrl());
        Assert.assertNull(grpcUpstream.getProtocol());
    }

    @Test
    public void buildAliveGrpcUpstream() {
        GrpcUpstream grpcUpstream = CommonUpstreamUtils.buildAliveGrpcUpstream(HOST);
        Assert.assertNotNull(grpcUpstream);
        Assert.assertEquals(HOST, grpcUpstream.getUpstreamUrl());
        Assert.assertNull(grpcUpstream.getProtocol());
    }

    @Test
    public void convertCommonUpstreamList() {
        List<DivideUpstream> existDivideUpstreams = new ArrayList<>();
        DivideUpstream divideUpstream = CommonUpstreamUtils.buildDivideUpstream("http", HOST, PORT, SYS_DEFAULT_NAMESPACE_ID, EventType.REGISTER);
        existDivideUpstreams.add(divideUpstream);

        List<CommonUpstream> commonUpstreams = CommonUpstreamUtils.convertCommonUpstreamList(existDivideUpstreams);
        Assert.assertNotNull(commonUpstreams);
        Assert.assertEquals(1, commonUpstreams.size());
        Assert.assertEquals("http", commonUpstreams.get(0).getProtocol());
        Assert.assertEquals(HOST + ":" + PORT, commonUpstreams.get(0).getUpstreamUrl());
        Assert.assertNotNull(CommonUpstreamUtils.convertCommonUpstreamList(null));
        Assert.assertNotNull(CommonUpstreamUtils.convertCommonUpstreamList(new ArrayList<>()));
    }

    @Test
    public void buildUrl() {
        String url = CommonUpstreamUtils.buildUrl(HOST, PORT);
        Assert.assertEquals(HOST + ":" + PORT, url);
    }

    @Test
    public void testBuildDefaultDivideUpstreamWithNullPort() {
        // Test case: null port - should mark status as false
        DivideUpstream result = CommonUpstreamUtils.buildDefaultDivideUpstream(HOST, null, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
        Assert.assertEquals("localhost", result.getUpstreamHost());
        Assert.assertEquals("http://", result.getProtocol());
    }

    @Test
    public void testBuildDefaultDivideUpstreamWithBlankHost() {
        // Test case: blank host - should mark status as false
        DivideUpstream result = CommonUpstreamUtils.buildDefaultDivideUpstream("", PORT, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildDefaultDivideUpstreamWithNullHostAndPort() {
        // Test case: null host and port
        DivideUpstream result = CommonUpstreamUtils.buildDefaultDivideUpstream(null, null, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildDefaultDiscoveryUpstreamDTO() {
        // Test case: normal case
        String protocol = "http";
        DiscoveryUpstreamDTO result = CommonUpstreamUtils.buildDefaultDiscoveryUpstreamDTO(HOST, PORT, protocol, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(result);
        Assert.assertEquals(HOST + ":" + PORT, result.getUrl());
        Assert.assertEquals(protocol, result.getProtocol());
        Assert.assertEquals(0, result.getStatus());
        Assert.assertEquals(Long.valueOf(50), Long.valueOf(result.getWeight()));
        Assert.assertEquals(SYS_DEFAULT_NAMESPACE_ID, result.getNamespaceId());
        Assert.assertNotNull(result.getProps());
    }

    @Test
    public void testBuildDefaultDiscoveryUpstreamDTOWithGrpcProtocol() {
        // Test case: grpc protocol
        String protocol = "grpc";
        DiscoveryUpstreamDTO result = CommonUpstreamUtils.buildDefaultDiscoveryUpstreamDTO(HOST, PORT, protocol, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(result);
        Assert.assertEquals(protocol, result.getProtocol());
    }

    @Test
    public void testBuildDivideUpstreamWithDeletedEvent() {
        // Test case: DELETED event - should mark status as false
        DivideUpstream result = CommonUpstreamUtils.buildDivideUpstream("http://", HOST, PORT, SYS_DEFAULT_NAMESPACE_ID, EventType.DELETED);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildDivideUpstreamWithOfflineEvent() {
        // Test case: OFFLINE event - should mark status as false
        DivideUpstream result = CommonUpstreamUtils.buildDivideUpstream("http://", HOST, PORT, SYS_DEFAULT_NAMESPACE_ID, EventType.OFFLINE);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildDivideUpstreamWithIgnoredEvent() {
        // Test case: IGNORED event - should mark status as false
        DivideUpstream result = CommonUpstreamUtils.buildDivideUpstream("http://", HOST, PORT, SYS_DEFAULT_NAMESPACE_ID, EventType.IGNORED);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildDivideUpstreamWithNullPort() {
        // Test case: null port - should mark status as false
        DivideUpstream result = CommonUpstreamUtils.buildDivideUpstream("http://", HOST, null, SYS_DEFAULT_NAMESPACE_ID, EventType.REGISTER);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildDivideUpstreamWithBlankHost() {
        // Test case: blank host - should mark status as false
        DivideUpstream result = CommonUpstreamUtils.buildDivideUpstream("http://", "", PORT, SYS_DEFAULT_NAMESPACE_ID, EventType.REGISTER);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildWebSocketUpstreamWithNullPort() {
        // Test case: null port - should mark status as false
        WebSocketUpstream result = CommonUpstreamUtils.buildWebSocketUpstream("ws://", HOST, null, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
        Assert.assertEquals("localhost", result.getHost());
    }

    @Test
    public void testBuildWebSocketUpstreamWithBlankHost() {
        // Test case: blank host - should mark status as false
        WebSocketUpstream result = CommonUpstreamUtils.buildWebSocketUpstream("ws://", "", PORT, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildDefaultDubboUpstreamWithNullPort() {
        // Test case: null port - should mark status as false
        DubboUpstream result = CommonUpstreamUtils.buildDefaultDubboUpstream(HOST, null);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
        Assert.assertEquals("dubbo://", result.getProtocol());
    }

    @Test
    public void testBuildDefaultDubboUpstreamWithBlankHost() {
        // Test case: blank host - should mark status as false
        DubboUpstream result = CommonUpstreamUtils.buildDefaultDubboUpstream("", PORT);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildDefaultGrpcUpstreamWithNullPort() {
        // Test case: null port - should mark status as false
        GrpcUpstream result = CommonUpstreamUtils.buildDefaultGrpcUpstream(HOST, null, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildDefaultGrpcUpstreamWithBlankHost() {
        // Test case: blank host - should mark status as false
        GrpcUpstream result = CommonUpstreamUtils.buildDefaultGrpcUpstream("", PORT, SYS_DEFAULT_NAMESPACE_ID);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildDefaultTarsUpstream() {
        // Test case: valid data
        TarsUpstream result = CommonUpstreamUtils.buildDefaultTarsUpstream(HOST, PORT);
        Assert.assertNotNull(result);
        Assert.assertEquals(HOST + ":" + PORT, result.getUpstreamUrl());
        Assert.assertEquals(Long.valueOf(50), Long.valueOf(result.getWeight()));
        Assert.assertEquals(Long.valueOf(Constants.WARMUP_TIME), Long.valueOf(result.getWarmup()));
        Assert.assertTrue(result.isStatus());
    }

    @Test
    public void testBuildDefaultTarsUpstreamWithNullPort() {
        // Test case: null port - should mark status as false
        TarsUpstream result = CommonUpstreamUtils.buildDefaultTarsUpstream(HOST, null);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isStatus());
    }

    @Test
    public void testBuildAliveTarsUpstream() {
        // Test case: normal case
        String upstreamUrl = HOST + ":" + PORT;
        TarsUpstream result = CommonUpstreamUtils.buildAliveTarsUpstream(upstreamUrl);
        Assert.assertNotNull(result);
        Assert.assertEquals(upstreamUrl, result.getUpstreamUrl());
        Assert.assertEquals(Long.valueOf(50), Long.valueOf(result.getWeight()));
    }

    @Test
    public void testConvertCommonUpstreamListWithHealthCheck() {
        // Test case: list with health check enabled
        WebSocketUpstream upstream = WebSocketUpstream.builder()
                .protocol("ws://")
                .host("localhost")
                .upstreamUrl("ws://localhost:8080")
                .status(true)
                .timestamp(System.currentTimeMillis())
                .build();
        upstream.setHealthCheckEnabled(true);

        List<WebSocketUpstream> upstreamList = new ArrayList<>();
        upstreamList.add(upstream);

        List<CommonUpstream> result = CommonUpstreamUtils.convertCommonUpstreamList(upstreamList);
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        Assert.assertTrue(result.get(0).isHealthCheckEnabled());
    }

    @Test
    public void testBuildDefaultAliveDivideUpstream() {
        // Test case: normal case
        DivideUpstream result = CommonUpstreamUtils.buildDefaultAliveDivideUpstream(HOST);
        Assert.assertNotNull(result);
        Assert.assertEquals("localhost", result.getUpstreamHost());
        Assert.assertEquals("http://", result.getProtocol());
        Assert.assertEquals(HOST, result.getUpstreamUrl());
        Assert.assertEquals(Long.valueOf(50), Long.valueOf(result.getWeight()));
    }

    @Test
    public void testBuildUrlWithZeroPort() {
        // Test case: zero port
        String url = CommonUpstreamUtils.buildUrl(HOST, 0);
        Assert.assertEquals(HOST + ":0", url);
    }

    @Test
    public void testBuildDefaultDivideUpstreamTimestamp() {
        // Test case: verify timestamp is set
        long beforeTime = System.currentTimeMillis();
        DivideUpstream result = CommonUpstreamUtils.buildDefaultDivideUpstream(HOST, PORT, SYS_DEFAULT_NAMESPACE_ID);
        long afterTime = System.currentTimeMillis();

        Assert.assertNotNull(result);
        Assert.assertTrue(result.getTimestamp() >= beforeTime);
        Assert.assertTrue(result.getTimestamp() <= afterTime);
    }
}
