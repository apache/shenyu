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
import org.apache.shenyu.common.dto.convert.selector.TarsUpstream;
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
 * Test case for {@link TarsSelectorHandleConverter}.
 */
class TarsSelectorHandleConverterTest {

    private TarsSelectorHandleConverter converter;

    @BeforeEach
    void setUp() {
        converter = new TarsSelectorHandleConverter();
    }

    @Test
    void testPluginName() {
        String pluginName = converter.pluginName();
        assertThat(pluginName, is(PluginEnum.TARS.getName()));
        assertEquals("tars", pluginName);
    }

    @Test
    void testConvertUpstream() {
        TarsUpstream upstream1 = buildTarsUpstream("localhost:8080", true, 100, 10);
        TarsUpstream upstream2 = buildTarsUpstream("localhost:8081", true, 50, 5);
        List<TarsUpstream> upstreamList = new ArrayList<>();
        upstreamList.add(upstream1);
        upstreamList.add(upstream2);

        String handle = GsonUtils.getInstance().toJson(upstreamList);
        List<CommonUpstream> result = converter.convertUpstream(handle);

        assertThat(result, notNullValue());
        assertThat(result, hasSize(2));
        assertEquals("localhost:8080", result.get(0).getUpstreamUrl());
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
        TarsUpstream upstream = buildTarsUpstream("localhost:8080", true, 100, 10);
        String handle = GsonUtils.getInstance().toJson(Collections.singletonList(upstream));

        String result = converter.handler(handle, Collections.emptyList());

        assertNotNull(result);
        List<TarsUpstream> resultList = GsonUtils.getInstance().fromList(result, TarsUpstream.class);
        assertThat(resultList, hasSize(0));
    }

    @Test
    void testHandlerWithEmptyHandleAndAliveList() {
        CommonUpstream aliveUpstream = buildCommonUpstream("localhost:8080");
        List<CommonUpstream> aliveList = Collections.singletonList(aliveUpstream);

        String result = converter.handler("", aliveList);

        assertNotNull(result);
        List<TarsUpstream> resultList = GsonUtils.getInstance().fromList(result, TarsUpstream.class);
        assertThat(resultList, hasSize(1));
        assertEquals("localhost:8080", resultList.get(0).getUpstreamUrl());
    }

    @Test
    void testHandlerWithExistingAndNewAliveUpstreams() {
        TarsUpstream existingUpstream = buildTarsUpstream("localhost:8080", true, 100, 10);
        String handle = GsonUtils.getInstance().toJson(Collections.singletonList(existingUpstream));

        CommonUpstream aliveUpstream1 = buildCommonUpstream("localhost:8080");
        CommonUpstream aliveUpstream2 = buildCommonUpstream("localhost:8081");
        List<CommonUpstream> aliveList = new ArrayList<>();
        aliveList.add(aliveUpstream1);
        aliveList.add(aliveUpstream2);

        String result = converter.handler(handle, aliveList);

        assertNotNull(result);
        List<TarsUpstream> resultList = GsonUtils.getInstance().fromList(result, TarsUpstream.class);
        assertThat(resultList, hasSize(2));
        assertTrue(resultList.stream().anyMatch(u -> "localhost:8080".equals(u.getUpstreamUrl())));
        assertTrue(resultList.stream().anyMatch(u -> "localhost:8081".equals(u.getUpstreamUrl())));
    }

    @Test
    void testHandlerRemovesDeadUpstream() {
        TarsUpstream existingUpstream1 = buildTarsUpstream("localhost:8080", true, 100, 10);
        TarsUpstream existingUpstream2 = buildTarsUpstream("localhost:8081", true, 50, 5);
        List<TarsUpstream> existingList = new ArrayList<>();
        existingList.add(existingUpstream1);
        existingList.add(existingUpstream2);
        String handle = GsonUtils.getInstance().toJson(existingList);

        CommonUpstream aliveUpstream = buildCommonUpstream("localhost:8080");
        List<CommonUpstream> aliveList = Collections.singletonList(aliveUpstream);

        String result = converter.handler(handle, aliveList);

        assertNotNull(result);
        List<TarsUpstream> resultList = GsonUtils.getInstance().fromList(result, TarsUpstream.class);
        assertThat(resultList, hasSize(1));
        assertEquals("localhost:8080", resultList.get(0).getUpstreamUrl());
        assertTrue(resultList.get(0).isStatus());
    }

    @Test
    void testHandlerWithMultipleScenarios() {
        TarsUpstream existingUpstream = buildTarsUpstream("localhost:8080", false, 100, 10);
        existingUpstream.setTimestamp(System.currentTimeMillis());
        String handle = GsonUtils.getInstance().toJson(Collections.singletonList(existingUpstream));

        CommonUpstream aliveUpstream1 = buildCommonUpstream("localhost:8080");
        CommonUpstream aliveUpstream2 = buildCommonUpstream("localhost:8082");
        List<CommonUpstream> aliveList = new ArrayList<>();
        aliveList.add(aliveUpstream1);
        aliveList.add(aliveUpstream2);

        String result = converter.handler(handle, aliveList);

        assertNotNull(result);
        List<TarsUpstream> resultList = GsonUtils.getInstance().fromList(result, TarsUpstream.class);
        assertThat(resultList, hasSize(2));

        TarsUpstream revived = resultList.stream()
                .filter(u -> "localhost:8080".equals(u.getUpstreamUrl()))
                .findFirst()
                .orElse(null);
        assertNotNull(revived);
        assertTrue(revived.isStatus());

        TarsUpstream newUpstream = resultList.stream()
                .filter(u -> "localhost:8082".equals(u.getUpstreamUrl()))
                .findFirst()
                .orElse(null);
        assertNotNull(newUpstream);
    }

    @Test
    void testHandlerPreservesWeightAndWarmup() {
        TarsUpstream existingUpstream = buildTarsUpstream("localhost:8080", true, 80, 20);
        String handle = GsonUtils.getInstance().toJson(Collections.singletonList(existingUpstream));

        CommonUpstream aliveUpstream = buildCommonUpstream("localhost:8080");
        List<CommonUpstream> aliveList = Collections.singletonList(aliveUpstream);

        String result = converter.handler(handle, aliveList);

        List<TarsUpstream> resultList = GsonUtils.getInstance().fromList(result, TarsUpstream.class);
        assertThat(resultList, hasSize(1));
        assertEquals(80, resultList.get(0).getWeight());
        assertEquals(20, resultList.get(0).getWarmup());
    }

    @Test
    void testHandlerRemovesAllDeadUpstreams() {
        TarsUpstream existingUpstream1 = buildTarsUpstream("localhost:8080", true, 100, 10);
        TarsUpstream existingUpstream2 = buildTarsUpstream("localhost:8081", true, 50, 5);
        TarsUpstream existingUpstream3 = buildTarsUpstream("localhost:8082", true, 60, 8);
        List<TarsUpstream> existingList = new ArrayList<>();
        existingList.add(existingUpstream1);
        existingList.add(existingUpstream2);
        existingList.add(existingUpstream3);
        String handle = GsonUtils.getInstance().toJson(existingList);

        CommonUpstream aliveUpstream = buildCommonUpstream("localhost:8081");
        List<CommonUpstream> aliveList = Collections.singletonList(aliveUpstream);

        String result = converter.handler(handle, aliveList);

        assertNotNull(result);
        List<TarsUpstream> resultList = GsonUtils.getInstance().fromList(result, TarsUpstream.class);
        assertThat(resultList, hasSize(1));
        assertEquals("localhost:8081", resultList.get(0).getUpstreamUrl());
        assertFalse(resultList.stream().anyMatch(u -> "localhost:8080".equals(u.getUpstreamUrl())));
        assertFalse(resultList.stream().anyMatch(u -> "localhost:8082".equals(u.getUpstreamUrl())));
    }

    private TarsUpstream buildTarsUpstream(final String url, final boolean status,
                                            final int weight, final int warmup) {
        return TarsUpstream.builder()
                .upstreamUrl(url)
                .status(status)
                .weight(weight)
                .warmup(warmup)
                .timestamp(System.currentTimeMillis())
                .build();
    }

    private CommonUpstream buildCommonUpstream(final String url) {
        return new CommonUpstream("", "localhost", url, true, System.currentTimeMillis());
    }
}
