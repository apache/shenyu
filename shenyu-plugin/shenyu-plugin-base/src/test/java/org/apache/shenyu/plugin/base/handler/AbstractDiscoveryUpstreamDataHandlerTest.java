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

package org.apache.shenyu.plugin.base.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamKey;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link AbstractDiscoveryUpstreamDataHandler}.
 */
class AbstractDiscoveryUpstreamDataHandlerTest {

    @Test
    void testIgnoreNullDiscoverySyncData() {
        final TestHandler handler = new TestHandler();

        handler.handlerDiscoveryUpstreamData(null);
        final DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        handler.handlerDiscoveryUpstreamData(discoverySyncData);

        assertTrue(handler.submitted.isEmpty());
    }

    @Test
    void testSubmitAllUpstreamsWhenNoGrayUpstream() {
        final TestHandler handler = new TestHandler();

        handler.handlerDiscoveryUpstreamData(discoverySyncData("a", "b"));

        assertEquals(List.of("a", "b"), handler.submitted);
        assertEquals(1, handler.afterSubmitCount);
    }

    @Test
    void testSubmitOnlyGrayUpstreamsWhenPresent() {
        final TestHandler handler = new TestHandler();
        handler.grayUrl = "gray";

        handler.handlerDiscoveryUpstreamData(discoverySyncData("gray", "normal"));

        assertEquals(List.of("gray"), handler.submitted);
    }

    @Test
    void testRemoveDiscoveryUpstreamData() {
        final TestHandler handler = new TestHandler();

        handler.removeDiscoveryUpstreamData(null);
        handler.removeDiscoveryUpstreamData(new DiscoveryUpstreamKey("test", null, null));
        assertTrue(handler.removed.isEmpty());

        handler.removeDiscoveryUpstreamData(new DiscoveryUpstreamKey("test", "selector", null));
        assertEquals(List.of("selector"), handler.removed);
    }

    private DiscoverySyncData discoverySyncData(final String... urls) {
        final DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setSelectorId("selector");
        discoverySyncData.setUpstreamDataList(Arrays.stream(urls).map(this::upstream).collect(Collectors.toList()));
        return discoverySyncData;
    }

    private DiscoveryUpstreamData upstream(final String url) {
        final DiscoveryUpstreamData upstream = new DiscoveryUpstreamData();
        upstream.setUrl(url);
        return upstream;
    }

    private static final class TestHandler extends AbstractDiscoveryUpstreamDataHandler<String> {

        private final List<String> submitted = new ArrayList<>();

        private final List<String> removed = new ArrayList<>();

        private String grayUrl;

        private int afterSubmitCount;

        @Override
        protected List<String> convertUpstreamList(final List<DiscoveryUpstreamData> upstreamList) {
            return upstreamList.stream().map(DiscoveryUpstreamData::getUrl).collect(Collectors.toList());
        }

        @Override
        protected boolean isGray(final String upstream) {
            return Objects.equals(grayUrl, upstream);
        }

        @Override
        protected void submitUpstreamData(final String selectorId, final List<String> upstreamList) {
            submitted.addAll(upstreamList);
        }

        @Override
        protected void afterSubmitUpstreamData(final String selectorId) {
            afterSubmitCount++;
        }

        @Override
        protected void removeUpstreamData(final String selectorId) {
            removed.add(selectorId);
        }

        @Override
        public String pluginName() {
            return "test";
        }
    }
}
