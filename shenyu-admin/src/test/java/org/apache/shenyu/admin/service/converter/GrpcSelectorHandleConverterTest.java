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

package org.apache.shenyu.admin.service.converter;

import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link GrpcSelectorHandleConverter}.
 */
class GrpcSelectorHandleConverterTest {

    private GrpcSelectorHandleConverter converter;

    @BeforeEach
    void setUp() {
        converter = new GrpcSelectorHandleConverter();
    }

    @Test
    void testPluginName() {
        String pluginName = converter.pluginName();
        assertThat(pluginName, is(PluginEnum.GRPC.getName()));
        assertEquals("grpc", pluginName);
    }

    @Test
    void testConvertUpstream() {
        GrpcUpstream upstream1 = buildGrpcUpstream("localhost:9090", true, 100);
        GrpcUpstream upstream2 = buildGrpcUpstream("localhost:9091", true, 50);
        List<GrpcUpstream> upstreamList = new ArrayList<>();
        upstreamList.add(upstream1);
        upstreamList.add(upstream2);

        String handle = GsonUtils.getInstance().toJson(upstreamList);
        List<CommonUpstream> result = converter.convertUpstream(handle);

        assertThat(result, notNullValue());
        assertThat(result, hasSize(2));
        assertEquals("localhost:9090", result.get(0).getUpstreamUrl());
        assertTrue(result.get(0).isStatus());
    }

    @Test
    void testConvertUpstreamWithEmptyHandle() {
        List<CommonUpstream> result = converter.convertUpstream("[]");
        assertThat(result, notNullValue());
        assertThat(result, hasSize(0));
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"[]"})
    void testHandlerWithEmptyInputs(final String handle) {
        String result = converter.handler(handle, Collections.emptyList());
        assertEquals("[]", result);
    }

    @Test
    void testHandlerWithValidHandleAndEmptyAliveList() {
        GrpcUpstream upstream = buildGrpcUpstream("localhost:9090", true, 100);
        String handle = GsonUtils.getInstance().toJson(Collections.singletonList(upstream));

        String result = converter.handler(handle, Collections.emptyList());

        assertNotNull(result);
        List<GrpcUpstream> resultList = GsonUtils.getInstance().fromList(result, GrpcUpstream.class);
        assertThat(resultList, hasSize(0));
    }

    @Test
    void testHandlerWithEmptyHandleAndAliveList() {
        CommonUpstream aliveUpstream = buildCommonUpstream("localhost:9090");
        List<CommonUpstream> aliveList = Collections.singletonList(aliveUpstream);

        String result = converter.handler("", aliveList);

        assertNotNull(result);
        List<GrpcUpstream> resultList = GsonUtils.getInstance().fromList(result, GrpcUpstream.class);
        assertThat(resultList, hasSize(1));
        assertEquals("localhost:9090", resultList.get(0).getUpstreamUrl());
    }

    @Test
    void testHandlerWithExistingAndNewAliveUpstreams() {
        GrpcUpstream existingUpstream = buildGrpcUpstream("localhost:9090", true, 100);
        String handle = GsonUtils.getInstance().toJson(Collections.singletonList(existingUpstream));

        CommonUpstream aliveUpstream1 = buildCommonUpstream("localhost:9090");
        CommonUpstream aliveUpstream2 = buildCommonUpstream("localhost:9091");
        List<CommonUpstream> aliveList = new ArrayList<>();
        aliveList.add(aliveUpstream1);
        aliveList.add(aliveUpstream2);

        String result = converter.handler(handle, aliveList);

        assertNotNull(result);
        List<GrpcUpstream> resultList = GsonUtils.getInstance().fromList(result, GrpcUpstream.class);
        assertThat(resultList, hasSize(2));
        assertTrue(resultList.stream().anyMatch(u -> "localhost:9090".equals(u.getUpstreamUrl())));
        assertTrue(resultList.stream().anyMatch(u -> "localhost:9091".equals(u.getUpstreamUrl())));
    }

    @Test
    void testHandlerRemovesDeadUpstream() {
        GrpcUpstream existingUpstream1 = buildGrpcUpstream("localhost:9090", true, 100);
        GrpcUpstream existingUpstream2 = buildGrpcUpstream("localhost:9091", true, 50);
        List<GrpcUpstream> existingList = new ArrayList<>();
        existingList.add(existingUpstream1);
        existingList.add(existingUpstream2);
        String handle = GsonUtils.getInstance().toJson(existingList);

        CommonUpstream aliveUpstream = buildCommonUpstream("localhost:9090");
        List<CommonUpstream> aliveList = Collections.singletonList(aliveUpstream);

        String result = converter.handler(handle, aliveList);

        assertNotNull(result);
        List<GrpcUpstream> resultList = GsonUtils.getInstance().fromList(result, GrpcUpstream.class);
        assertThat(resultList, hasSize(1));
        assertEquals("localhost:9090", resultList.get(0).getUpstreamUrl());
        assertTrue(resultList.get(0).isStatus());
    }

    @Test
    void testHandlerWithMultipleScenarios() {
        GrpcUpstream existingUpstream = buildGrpcUpstream("localhost:9090", false, 100);
        existingUpstream.setTimestamp(System.currentTimeMillis());
        String handle = GsonUtils.getInstance().toJson(Collections.singletonList(existingUpstream));

        CommonUpstream aliveUpstream1 = buildCommonUpstream("localhost:9090");
        CommonUpstream aliveUpstream2 = buildCommonUpstream("localhost:9092");
        List<CommonUpstream> aliveList = new ArrayList<>();
        aliveList.add(aliveUpstream1);
        aliveList.add(aliveUpstream2);

        String result = converter.handler(handle, aliveList);

        assertNotNull(result);
        List<GrpcUpstream> resultList = GsonUtils.getInstance().fromList(result, GrpcUpstream.class);
        assertThat(resultList, hasSize(2));

        GrpcUpstream revived = resultList.stream()
                .filter(u -> "localhost:9090".equals(u.getUpstreamUrl()))
                .findFirst()
                .orElse(null);
        assertNotNull(revived);
        assertTrue(revived.isStatus());

        GrpcUpstream newUpstream = resultList.stream()
                .filter(u -> "localhost:9092".equals(u.getUpstreamUrl()))
                .findFirst()
                .orElse(null);
        assertNotNull(newUpstream);
    }

    @Test
    void testHandlerPreservesWeight() {
        GrpcUpstream existingUpstream = buildGrpcUpstream("localhost:9090", true, 80);
        String handle = GsonUtils.getInstance().toJson(Collections.singletonList(existingUpstream));

        CommonUpstream aliveUpstream = buildCommonUpstream("localhost:9090");
        List<CommonUpstream> aliveList = Collections.singletonList(aliveUpstream);

        String result = converter.handler(handle, aliveList);

        List<GrpcUpstream> resultList = GsonUtils.getInstance().fromList(result, GrpcUpstream.class);
        assertThat(resultList, hasSize(1));
        assertEquals(80, resultList.get(0).getWeight());
    }

    @Test
    void testHandlerRemovesAllDeadUpstreams() {
        GrpcUpstream existingUpstream1 = buildGrpcUpstream("localhost:9090", true, 100);
        GrpcUpstream existingUpstream2 = buildGrpcUpstream("localhost:9091", true, 50);
        GrpcUpstream existingUpstream3 = buildGrpcUpstream("localhost:9092", true, 60);
        List<GrpcUpstream> existingList = new ArrayList<>();
        existingList.add(existingUpstream1);
        existingList.add(existingUpstream2);
        existingList.add(existingUpstream3);
        String handle = GsonUtils.getInstance().toJson(existingList);

        CommonUpstream aliveUpstream = buildCommonUpstream("localhost:9091");
        List<CommonUpstream> aliveList = Collections.singletonList(aliveUpstream);

        String result = converter.handler(handle, aliveList);

        assertNotNull(result);
        List<GrpcUpstream> resultList = GsonUtils.getInstance().fromList(result, GrpcUpstream.class);
        assertThat(resultList, hasSize(1));
        assertEquals("localhost:9091", resultList.get(0).getUpstreamUrl());
        assertFalse(resultList.stream().anyMatch(u -> "localhost:9090".equals(u.getUpstreamUrl())));
        assertFalse(resultList.stream().anyMatch(u -> "localhost:9092".equals(u.getUpstreamUrl())));
    }

    private GrpcUpstream buildGrpcUpstream(final String url, final boolean status, final int weight) {
        return GrpcUpstream.builder()
                .upstreamUrl(url)
                .status(status)
                .weight(weight)
                .timestamp(System.currentTimeMillis())
                .build();
    }

    private CommonUpstream buildCommonUpstream(final String url) {
        return new CommonUpstream("", "localhost", url, true, System.currentTimeMillis());
    }
}
