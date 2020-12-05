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

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.service.impl.EnumServiceImpl;
import org.dromara.soul.admin.vo.EnumVO;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.junit.MockitoJUnitRunner;
import java.util.List;
import java.util.Map;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.comparesEqualTo;
import static org.hamcrest.number.OrderingComparison.greaterThan;
import static org.junit.Assert.assertThat;

/**
 * Test cases for EnumService.
 *
 * @author onlyonezhongjinhui
 */
@RunWith(MockitoJUnitRunner.class)
public final class EnumServiceTest {

    @InjectMocks
    private EnumServiceImpl enumService;

    @Test
    public void testListSize() {
        Map<String, List<EnumVO>> list = enumService.list();
        assertThat(list.size(), greaterThan(0));
    }

    @Test
    public void testListHttpMethodEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "httpMethodEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> assertThat(e.getCode(), nullValue()));
    }

    @Test
    public void testListLoadBalanceEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "loadBalanceEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> assertThat(e.getSupport(), comparesEqualTo(true)));
    }

    @Test
    public void testListMatchModeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "matchModeEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> assertThat(e.getSupport(), comparesEqualTo(true)));
    }

    @Test
    public void testListOperatorEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "operatorEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> assertThat(e.getCode(), nullValue()));
    }

    @Test
    public void testListParamTypeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "paramTypeEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> assertThat(e.getCode(), nullValue()));
    }

    @Test
    public void testListPluginEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "pluginEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> assertThat(e.getSupport(), comparesEqualTo(true)));
    }

    @Test
    public void testListPluginTypeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "pluginTypeEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> {
            assertThat(e.getCode(), nullValue());
            assertThat(e.getSupport(), comparesEqualTo(true));
        });
    }

    @Test
    public void testListRpcTypeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "rpcTypeEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> assertThat(e.getCode(), nullValue()));
    }

    @Test
    public void testListSelectorTypeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "selectorTypeEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> assertThat(e.getSupport(), comparesEqualTo(true)));
    }

    @Test
    public void testListSerializeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "serializeEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> {
            assertThat(e.getCode(), nullValue());
            assertThat(e.getSupport(), comparesEqualTo(true));
        });
    }

    @Test
    public void testListWafEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "wafEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> assertThat(e.getSupport(), comparesEqualTo(true)));
    }

    @Test
    public void testListRedisModeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "redisModeEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> {
            assertThat(e.getCode(), nullValue());
            assertThat(e.getSupport(), comparesEqualTo(true));
        });
    }

    @Test
    public void testListHystrixIsolationModeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        String key = "hystrixIsolationModeEnums";
        assertThat(list, hasKey(key));

        List<EnumVO> enums = list.get(key);
        assertThat(enums.size(), greaterThan(0));
        enums.forEach(e -> assertThat(e.getSupport(), comparesEqualTo(true)));
    }

}
