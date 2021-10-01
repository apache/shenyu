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

import org.apache.shenyu.admin.service.register.ShenyuClientRegisterMotanServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;

/**
 * Test cases for ShenyuClientRegisterMotanServiceImpl.
 */
@RunWith(MockitoJUnitRunner.Silent.class)
public class ShenyuClientRegisterMotanServiceImplTest {
    @Mock
    private ShenyuClientRegisterMotanServiceImpl shenyuClientRegisterMotanService;

    @Mock
    private RuleService ruleService;

    @Mock
    private MetaDataService metaDataService;

    @Mock
    private SelectorService selectorService;

    @Before
    public void setup() {
        shenyuClientRegisterMotanService = new ShenyuClientRegisterMotanServiceImpl(metaDataService, selectorService, ruleService);
    }

    @Test
    public void testRegister() {
        MetaDataRegisterDTO dto = buildMetaDataRegisterDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterMotanService.register(dto));
    }

    private MetaDataRegisterDTO buildMetaDataRegisterDTO() {
        return MetaDataRegisterDTO.builder()
                .appName("appNameMetaData")
                .contextPath("contextPathMetaDataRegister")
                .path("/shenyu")
                .pathDesc("pathDescMetaData")
                .rpcType("rpcTypeMetaData")
                .serviceName("serviceName3")
                .methodName("methodName3")
                .ruleName("ruleNameMetaDataRegister")
                .parameterTypes("parameterTypesMetaData")
                .rpcExt("rpcExtMetaData")
                .enabled(true)
                .host("127.0.0.1")
                .port(22000)
                .pluginNames(new ArrayList<>())
                .registerMetaData(true)
                .build();
    }
}
