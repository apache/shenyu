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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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
        assertTrue(list.size() > 0);
    }

    @Test
    public void testListHttpMethodEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("httpMethodEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().noneMatch(e -> null != e.getCode()));
    }

    @Test
    public void testListLoadBalanceEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("loadBalanceEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().allMatch(EnumVO::getSupport));
    }

    @Test
    public void testListMatchModeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("matchModeEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().allMatch(EnumVO::getSupport));
    }

    @Test
    public void testListOperatorEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("operatorEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().noneMatch(e -> null != e.getCode()));
    }

    @Test
    public void testListParamTypeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("paramTypeEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().noneMatch(e -> null != e.getCode()));
    }

    @Test
    public void testListPluginEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("pluginEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().allMatch(EnumVO::getSupport));
    }

    @Test
    public void testListPluginTypeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("pluginTypeEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().noneMatch(e -> null != e.getCode()));
        assertTrue(enums.stream().allMatch(EnumVO::getSupport));
    }

    @Test
    public void testListRpcTypeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("rpcTypeEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().noneMatch(e -> null != e.getCode()));
    }

    @Test
    public void testListSelectorTypeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("selectorTypeEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().allMatch(EnumVO::getSupport));
    }

    @Test
    public void testListSerializeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("serializeEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().noneMatch(e -> null != e.getCode()));
        assertTrue(enums.stream().allMatch(EnumVO::getSupport));
    }

    @Test
    public void testListWafEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("wafEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().allMatch(EnumVO::getSupport));
    }

    @Test
    public void testListRedisModeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("redisModeEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().noneMatch(e -> null != e.getCode()));
        assertTrue(enums.stream().allMatch(EnumVO::getSupport));
    }

    @Test
    public void testListHystrixIsolationModeEnums() {
        Map<String, List<EnumVO>> list = enumService.list();
        List<EnumVO> enums = list.get("hystrixIsolationModeEnums");
        assertNotNull(list);
        assertTrue(enums.size() > 0);
        assertTrue(enums.stream().allMatch(EnumVO::getSupport));
    }

}
