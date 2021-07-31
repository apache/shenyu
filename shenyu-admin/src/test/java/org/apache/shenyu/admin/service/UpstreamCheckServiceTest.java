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

import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Test cases for UpstreamCheckService.
 */
@RunWith(MockitoJUnitRunner.class)
public final class UpstreamCheckServiceTest {

    private static final String MOCK_SELECTOR_NAME = "mockSelectorName";

    private static final String MOCK_SELECTOR_NAME_2 = "mockSelectorName2";

    private static final String MOCK_SELECTOR_NAME_OTHER = "mockSelectorNameOther";

    private UpstreamCheckService upstreamCheckService;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private SelectorConditionMapper selectorConditionMapper;

    private ShenyuRegisterCenterConfig shenyuRegisterCenterConfig = new ShenyuRegisterCenterConfig();

    private Map<String, List<DivideUpstream>> upstreamMap;

    @Before
    public void setUp() {
        shenyuRegisterCenterConfig.setRegisterType("http");

        //get static variable reference by reflection
        upstreamMap = (Map<String, List<DivideUpstream>>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP");
        upstreamCheckService = new UpstreamCheckService(selectorMapper, eventPublisher, pluginMapper, selectorConditionMapper, shenyuRegisterCenterConfig);
        //spring bean creation lifecycle phase:post construct.
        upstreamCheckService.setup();
    }

    @Test
    public void testRemoveByKey() {
        UpstreamCheckService.removeByKey(MOCK_SELECTOR_NAME);
        Assert.assertFalse(upstreamMap.containsKey(MOCK_SELECTOR_NAME));
    }

    @Test
    public void testSubmitWhenSelectorNameNotExists() {
        final DivideUpstream divideUpstream = DivideUpstream.builder()
                .upstreamUrl("divide-upstream-60")
                .weight(60)
                .build();
        upstreamCheckService.submit(MOCK_SELECTOR_NAME_OTHER, divideUpstream);
        Assert.assertTrue(upstreamMap.containsKey(MOCK_SELECTOR_NAME_OTHER));
    }

    @Test
    public void testReplace() {
        final DivideUpstream divideUpstream = DivideUpstream.builder()
                .upstreamHost("localhost")
                .build();
        final DivideUpstream divideUpstream2 = DivideUpstream.builder()
                .upstreamHost("localhost2")
                .build();
        upstreamCheckService.submit(MOCK_SELECTOR_NAME_2, divideUpstream);
        upstreamCheckService.replace(MOCK_SELECTOR_NAME_2, Collections.singletonList(divideUpstream2));
        Assert.assertEquals(1, upstreamMap.get(MOCK_SELECTOR_NAME_2).size());
        Assert.assertEquals("localhost2", upstreamMap.get(MOCK_SELECTOR_NAME_2).get(0).getUpstreamHost());
    }

    @Test
    public void testScheduled() {
        ReflectionTestUtils.invokeMethod(upstreamCheckService, "scheduled");
        Assert.assertFalse(upstreamMap.containsKey(MOCK_SELECTOR_NAME));
    }
}
