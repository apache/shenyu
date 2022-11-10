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

import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.WebSocketUpstream;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * Test case for {@link CommonUpstreamUtils}.
 */
public final class CommonUpstreamUtilsTest {

    private static final String HOST = "127.0.0.1";

    private static final Integer PORT = 8888;

    @Test
    public void buildDefaultDivideUpstreamWithHostAndPort() {
        DivideUpstream divideUpstream = CommonUpstreamUtils.buildDefaultDivideUpstream(HOST, PORT);
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
        DivideUpstream divideUpstream = CommonUpstreamUtils.buildDivideUpstream("http", HOST, PORT);
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
        WebSocketUpstream webSocketUpstream = CommonUpstreamUtils.buildWebSocketUpstream("tcp", HOST, PORT);
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
        GrpcUpstream grpcUpstream = CommonUpstreamUtils.buildDefaultGrpcUpstream(HOST, PORT);
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
        DivideUpstream divideUpstream = CommonUpstreamUtils.buildDivideUpstream("http", HOST, PORT);
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

}
