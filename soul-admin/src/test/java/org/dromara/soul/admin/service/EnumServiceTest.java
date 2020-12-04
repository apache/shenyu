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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

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
        assertEquals(13, list.size());
    }

    @Test
    public void testListEle() {
        Map<String, List<EnumVO>> list = enumService.list();
        assertNotNull(list.get("httpMethodEnums"));
        assertNotNull(list.get("loadBalanceEnums"));
        assertNotNull(list.get("matchModeEnums"));
        assertNotNull(list.get("operatorEnums"));
        assertNotNull(list.get("paramTypeEnums"));
        assertNotNull(list.get("pluginEnums"));
        assertNotNull(list.get("pluginTypeEnums"));
        assertNotNull(list.get("rpcTypeEnums"));
        assertNotNull(list.get("selectorTypeEnums"));
        assertNotNull(list.get("serializeEnums"));
        assertNotNull(list.get("wafEnums"));
        assertNotNull(list.get("redisModeEnums"));
        assertNotNull(list.get("hystrixIsolationModeEnums"));
    }

}
