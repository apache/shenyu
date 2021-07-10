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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.service.impl.EnumServiceImpl;
import org.apache.shenyu.admin.model.vo.EnumVO;
import org.junit.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.comparesEqualTo;
import static org.hamcrest.number.OrderingComparison.greaterThan;

/**
 * Test cases for EnumService.
 */
@RunWith(MockitoJUnitRunner.class)
public final class EnumServiceTest {

    private final EnumServiceImpl enumService = new EnumServiceImpl();

    @Test
    public void testListSize() {
        Map<String, List<EnumVO>> list = enumService.list();
        assertThat(list.size(), greaterThan(0));
    }

    /**
     * test element size.
     *
     * @param key element key
     */
    @ParameterizedTest(name = "{index} => test {0}")
    @MethodSource("nonNullEleKeys")
    public void testListEleSize(final String key) {
        Map<String, List<EnumVO>> list = enumService.list();
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
    }

    /**
     * test element code.
     *
     * @param key element key
     */
    @ParameterizedTest(name = "{index} => test {0}")
    @MethodSource("nullEleCodeKeys")
    public void testListEleCode(final String key) {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get(key);
        enums.forEach(e -> assertThat(e.getCode(), nullValue()));
    }

    /**
     * test element support.
     *
     * @param key element key
     */
    @ParameterizedTest(name = "{index} => test {0}")
    @MethodSource("supportEleCodeKeys")
    public void testListEleSupport(final String key) {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get(key);
        enums.forEach(e -> assertThat(e.getSupport(), comparesEqualTo(true)));
    }

    private static Stream<String> nonNullEleKeys() {
        return Stream.of("httpMethodEnums", "loadBalanceEnums", "matchModeEnums", "operatorEnums", "paramTypeEnums",
                "pluginEnums", "pluginTypeEnums", "rpcTypeEnums", "selectorTypeEnums", "serializeEnums", "wafEnums",
                "redisModeEnums", "hystrixIsolationModeEnums");
    }

    private static Stream<String> nullEleCodeKeys() {
        return Stream.of("httpMethodEnums", "operatorEnums", "paramTypeEnums", "pluginTypeEnums", "rpcTypeEnums",
                "serializeEnums", "redisModeEnums");
    }

    private static Stream<String> supportEleCodeKeys() {
        return Stream.of("loadBalanceEnums", "matchModeEnums", "pluginEnums", "pluginTypeEnums", "selectorTypeEnums",
                "serializeEnums", "wafEnums", "redisModeEnums", "hystrixIsolationModeEnums");
    }

}
