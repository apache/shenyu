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

import com.google.common.collect.Lists;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.converter.DivideSelectorHandleConverter;
import org.apache.shenyu.admin.service.converter.SelectorHandleConverter;
import org.apache.shenyu.admin.service.converter.SelectorHandleConverterFactor;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.ZombieUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ScheduledThreadPoolExecutor;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link UpstreamCheckService}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class UpstreamCheckServiceTest {

    private static final String MOCK_SELECTOR_NAME = "mockSelectorName";

    private static final String MOCK_SELECTOR_NAME_2 = "mockSelectorName2";

    private static final String MOCK_SELECTOR_NAME_OTHER = "mockSelectorNameOther";

    private static final String MOCK_PLUGIN_ID = "mockPluginId";

    private static Logger loggerSpy;

    private static MockedStatic<LoggerFactory> loggerFactoryMockedStatic;

    private UpstreamCheckService upstreamCheckService;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private SelectorConditionMapper selectorConditionMapper;

    private SelectorHandleConverterFactor converterFactor;
    
    @Mock
    private DiscoveryUpstreamService discoveryUpstreamService;

    private final ShenyuRegisterCenterConfig shenyuRegisterCenterConfig = new ShenyuRegisterCenterConfig();

    private Map<String, List<DivideUpstream>> upstreamMap;

    private Set<ZombieUpstream> zombieSet;

    @BeforeAll
    public static void beforeClass() {
        loggerSpy = spy(LoggerFactory.getLogger(UpstreamCheckService.class));
        loggerFactoryMockedStatic = mockStatic(LoggerFactory.class);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(UpstreamCheckService.class)).thenReturn(loggerSpy);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(anyString())).thenReturn(loggerSpy);
    }

    @AfterAll
    public static void afterClass() {
        loggerFactoryMockedStatic.close();
    }

    @BeforeEach
    public void setUp() {
        shenyuRegisterCenterConfig.setRegisterType("http");
        shenyuRegisterCenterConfig.getProps().setProperty(Constants.IS_CHECKED, "true");
        // get static variable reference by reflection
        upstreamMap = (Map<String, List<DivideUpstream>>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP");
        zombieSet = (Set<ZombieUpstream>) ReflectionTestUtils.getField(UpstreamCheckService.class, "ZOMBIE_SET");
        Map<String, SelectorHandleConverter> maps = new HashMap<>();
        maps.put(PluginEnum.DIVIDE.getName(), new DivideSelectorHandleConverter());
        converterFactor = new SelectorHandleConverterFactor(maps);
        upstreamCheckService = new UpstreamCheckService(selectorMapper, eventPublisher, pluginMapper, selectorConditionMapper,
                shenyuRegisterCenterConfig, converterFactor, discoveryUpstreamService);
    }

    @Test
    @Disabled
    public void testScheduled() {
        PluginDO pluginDO = PluginDO.builder()
                .name(PluginEnum.DIVIDE.getName())
                .id(MOCK_PLUGIN_ID)
                .build();
        SelectorDO selectorDOWithUrlError = SelectorDO.builder()
                .pluginId(MOCK_PLUGIN_ID)
                .selectorName("UrlError")
                .handle("[{\"upstreamHost\":\"localhost\",\"protocol\":\"http://\",\"upstreamUrl\":\"divide-upstream-50\",\"weight\":50}]")
                .build();
        SelectorDO selectorDOWithUrlReachable = SelectorDO.builder()
                .pluginId(MOCK_PLUGIN_ID)
                .selectorName("UrlReachable")
                .handle("[{\"upstreamHost\":\"localhost\",\"protocol\":\"http://\",\"localhost\":\"divide-upstream-60\",\"weight\":60}]")
                .build();
        try (MockedStatic<UpstreamCheckUtils> mocked = mockStatic(UpstreamCheckUtils.class)) {
            mocked.when(() -> UpstreamCheckUtils.checkUrl("ReachableUrl"))
                    .thenReturn(true);
            mocked.when(() -> UpstreamCheckUtils.checkUrl("ErrorUrl"))
                    .thenReturn(false);

            zombieSet.clear();
            setupZombieSet();
            upstreamMap.clear();
            setupUpstreamMap();
            ReflectionTestUtils.invokeMethod(upstreamCheckService, "scheduled");
        }
        assertThat(zombieSet.size(), is(2));
        assertThat(upstreamMap.size(), is(2));
        assertTrue(upstreamMap.containsKey("UrlReachable"));
        assertTrue(upstreamMap.containsKey("UrlReachableAnother"));
    }

    @Test
    public void testRemoveByKey() {
        upstreamMap.put(MOCK_SELECTOR_NAME, Collections.emptyList());
        UpstreamCheckService.removeByKey(MOCK_SELECTOR_NAME);
        assertFalse(upstreamMap.containsKey(MOCK_SELECTOR_NAME));
    }

    @Test
    public void testSubmit() {
        ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-upstream-task", false));
        ReflectionTestUtils.setField(upstreamCheckService, "executor", executor);
        final DivideUpstream divideUpstream = DivideUpstream.builder()
                .upstreamUrl("divide-upstream-60")
                .weight(60)
                .build();
        // Test submit when selector name not exists
        testSubmitOnce(divideUpstream);
        // Test submit when selector name exists
        testSubmitOnce(divideUpstream);
        // Test service deleted
        divideUpstream.setStatus(false);
        testSubmitDeleted(divideUpstream);
        testSubmitDeleted(divideUpstream);
    }

    private void testSubmitOnce(final DivideUpstream divideUpstream) {
        upstreamCheckService.submit(MOCK_SELECTOR_NAME_OTHER, divideUpstream);
        assertTrue(upstreamMap.containsKey(MOCK_SELECTOR_NAME_OTHER));
    }

    private void testSubmitDeleted(final DivideUpstream divideUpstream) {
        upstreamCheckService.submit(MOCK_SELECTOR_NAME_OTHER, divideUpstream);
        assertFalse(upstreamMap.get(MOCK_SELECTOR_NAME_OTHER).contains(divideUpstream));
    }

    @Test
    public void testReplace() {
        ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-upstream-task", false));
        ReflectionTestUtils.setField(upstreamCheckService, "executor", executor);
        final DivideUpstream divideUpstream = DivideUpstream.builder()
                .upstreamHost("localhost")
                .build();
        final DivideUpstream divideUpstream2 = DivideUpstream.builder()
                .upstreamHost("localhost2")
                .build();
        upstreamCheckService.submit(MOCK_SELECTOR_NAME_2, divideUpstream);
        assertEquals(1, upstreamMap.get(MOCK_SELECTOR_NAME_2).size());
        assertEquals("localhost", upstreamMap.get(MOCK_SELECTOR_NAME_2).get(0).getUpstreamHost());
        upstreamCheckService.replace(MOCK_SELECTOR_NAME_2, Collections.singletonList(divideUpstream2));
        assertEquals(1, upstreamMap.get(MOCK_SELECTOR_NAME_2).size());
        assertEquals("localhost2", upstreamMap.get(MOCK_SELECTOR_NAME_2).get(0).getUpstreamHost());
    }

    @Test
    public void testFetchUpstreamData() {
        PluginDO pluginDO = PluginDO.builder()
                .name(PluginEnum.DIVIDE.getName())
                .id(MOCK_PLUGIN_ID)
                .build();
        SelectorDO selectorDOWithUrlError = SelectorDO.builder()
                .pluginId(MOCK_PLUGIN_ID)
                .id(MOCK_SELECTOR_NAME)
                .selectorName(MOCK_SELECTOR_NAME)
                .handle("[{\"upstreamHost\":\"localhost\",\"protocol\":\"http://\",\"upstreamUrl\":\"divide-upstream-50\",\"weight\":50}]")
                .build();
        SelectorDO selectorDOWithUrlReachable = SelectorDO.builder()
                .pluginId(MOCK_PLUGIN_ID)
                .id(MOCK_SELECTOR_NAME_OTHER)
                .selectorName(MOCK_SELECTOR_NAME_OTHER)
                .handle("[{\"upstreamHost\":\"localhost\",\"protocol\":\"http://\",\"localhost\":\"divide-upstream-60\",\"weight\":60}]")
                .build();
        DiscoveryUpstreamData discoveryUpstreamData = DiscoveryUpstreamData.builder()
                .dateCreated(new Timestamp(System.currentTimeMillis()))
                .protocol("http")
                .url("127.0.0.1:8080")
                .props("{}")
                .discoveryHandlerId("1")
                .status(0)
                .build();
        when(pluginMapper.selectByNames(anyList())).thenReturn(Lists.newArrayList(pluginDO));
        when(selectorMapper.findByPluginIds(anyList())).thenReturn(Lists.newArrayList(selectorDOWithUrlError, selectorDOWithUrlReachable));
        when(discoveryUpstreamService.findBySelectorId(anyString())).thenReturn(Lists.newArrayList(discoveryUpstreamData));
        upstreamCheckService.fetchUpstreamData();
        assertTrue(upstreamMap.containsKey(MOCK_SELECTOR_NAME));
        assertEquals(2, upstreamMap.get(MOCK_SELECTOR_NAME).size());
        assertTrue(upstreamMap.containsKey(MOCK_SELECTOR_NAME_OTHER));
        assertEquals(2, upstreamMap.get(MOCK_SELECTOR_NAME_OTHER).size());
    }

    @Test
    public void testClose() {
        Properties properties = new Properties();
        properties.setProperty(Constants.IS_CHECKED, "true");
        shenyuRegisterCenterConfig.setProps(properties);
        upstreamCheckService = new UpstreamCheckService(selectorMapper, eventPublisher, pluginMapper, selectorConditionMapper,
                shenyuRegisterCenterConfig, converterFactor, discoveryUpstreamService);
        upstreamCheckService.close();
    }

    private void setupZombieSet() {
        final DivideUpstream divideUpstream1 = DivideUpstream.builder()
                .upstreamHost("127.0.0.1")
                .upstreamUrl("ReachableUrl")
                .status(false)
                .build();
        final DivideUpstream divideUpstream2 = DivideUpstream.builder()
                .upstreamHost("ErrorUrl")
                .build();
        ZombieUpstream zombieUpstream1 = ZombieUpstream.builder()
                .commonUpstream(divideUpstream1)
                .zombieCheckTimes(5)
                .selectorId("UrlReachable")
                .build();
        ZombieUpstream zombieUpstream2 = ZombieUpstream.builder()
                .commonUpstream(divideUpstream2)
                .zombieCheckTimes(5)
                .selectorId("UrlError")
                .build();

        zombieSet.add(zombieUpstream1);
        zombieSet.add(zombieUpstream2);
    }

    private void setupUpstreamMap() {
        final DivideUpstream divideUpstream1 = DivideUpstream.builder()
                .upstreamHost("127.0.0.1")
                .upstreamUrl("ReachableUrl")
                .status(false)
                .build();
        final DivideUpstream divideUpstream2 = DivideUpstream.builder()
                .upstreamHost("ErrorUrl")
                .build();

        upstreamMap.put("UrlReachableAnother", Collections.singletonList(divideUpstream1));
        upstreamMap.put("UrlErrorAnother", Collections.singletonList(divideUpstream2));
    }
}
