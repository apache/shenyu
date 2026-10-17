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

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.converter.DivideSelectorHandleConverter;
import org.apache.shenyu.admin.service.converter.SelectorHandleConverterFactor;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.ZombieUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ScheduledThreadPoolExecutor;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Regression tests for recovery after asynchronous upstream check failures.
 */
@ExtendWith(MockitoExtension.class)
class UpstreamCheckServiceRecoveryTest {

    private static final String SELECTOR_ID = "recovery-selector";

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private SelectorConditionMapper selectorConditionMapper;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private DiscoveryUpstreamService discoveryUpstreamService;

    private UpstreamCheckService service;

    private List<CompletableFuture<Void>> futures;

    @BeforeEach
    @SuppressWarnings("unchecked")
    void setUp() {
        clearSharedState();
        ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
        config.setRegisterType("http");
        config.getProps().setProperty(Constants.IS_CHECKED, "true");
        SelectorHandleConverterFactor converterFactor = new SelectorHandleConverterFactor(
                Collections.singletonMap(PluginEnum.DIVIDE.getName(), new DivideSelectorHandleConverter()));
        service = new UpstreamCheckService(selectorMapper, eventPublisher, pluginMapper, selectorConditionMapper,
                config, converterFactor, discoveryUpstreamService);
        ReflectionTestUtils.setField(service, "invokeExecutor", new ScheduledThreadPoolExecutor(1));
        futures = (List<CompletableFuture<Void>>) ReflectionTestUtils.getField(service, "futures");
    }

    @AfterEach
    void tearDown() {
        service.close();
        clearSharedState();
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testRecoversAfterFailedAsyncUpdate(final boolean zombie) {
        final IllegalStateException failure = new IllegalStateException("temporary database failure");
        when(selectorMapper.selectById(SELECTOR_ID)).thenThrow(failure)
                .thenReturn(SelectorDO.builder().id(SELECTOR_ID).pluginId("divide-id").handle("[]").build());
        when(pluginMapper.selectById("divide-id"))
                .thenReturn(PluginDO.builder().id("divide-id").name(PluginEnum.DIVIDE.getName()).build());
        DivideUpstream upstream = DivideUpstream.builder().upstreamUrl("127.0.0.1:8080")
                .protocol("http://").healthCheckEnabled(false).status(false).build();
        if (zombie) {
            ReflectionTestUtils.invokeMethod(service, "checkZombie", ZombieUpstream.transform(upstream, 1, SELECTOR_ID));
        } else {
            service.replace(SELECTOR_ID, Collections.singletonList(upstream));
            ReflectionTestUtils.invokeMethod(service, "check", SELECTOR_ID, Collections.singletonList(upstream));
        }

        CompletionException exception = assertThrows(CompletionException.class, () -> ReflectionTestUtils.invokeMethod(service, "waitFinish"));
        assertSame(failure, exception.getCause());
        assertTrue(futures.isEmpty(), "The failed cycle must not retain its futures");

        // A new status change must be processed normally in the next scheduled cycle.
        upstream.setStatus(false);
        assertDoesNotThrow(() -> ReflectionTestUtils.invokeMethod(service, "scheduled"));
        assertTrue(futures.isEmpty());
        verify(selectorMapper, times(2)).selectById(SELECTOR_ID);
        verify(selectorMapper).updateSelective(any(SelectorDO.class));
        verify(eventPublisher, times(2)).publishEvent(any(DataChangedEvent.class));
        assertDoesNotThrow(() -> ReflectionTestUtils.invokeMethod(service, "waitFinish"));
    }

    @Test
    void testCancelledCheckDoesNotPoisonNextCycle() {
        CompletableFuture<Void> cancelled = new CompletableFuture<>();
        cancelled.cancel(false);
        futures.add(cancelled);

        assertThrows(CompletionException.class, () -> ReflectionTestUtils.invokeMethod(service, "waitFinish"));
        assertTrue(futures.isEmpty());
        futures.add(CompletableFuture.completedFuture(null));
        assertDoesNotThrow(() -> ReflectionTestUtils.invokeMethod(service, "waitFinish"));
        assertTrue(futures.isEmpty());
    }

    @Test
    void testSuccessfulAndEmptyCycles() {
        futures.add(CompletableFuture.completedFuture(null));
        futures.add(CompletableFuture.completedFuture(null));

        assertDoesNotThrow(() -> ReflectionTestUtils.invokeMethod(service, "waitFinish"));
        assertTrue(futures.isEmpty());
        assertDoesNotThrow(() -> ReflectionTestUtils.invokeMethod(service, "waitFinish"));
    }

    private static void clearSharedState() {
        ((Map<?, ?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP")).clear();
        ((Collection<?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "ZOMBIE_SET")).clear();
        ((Collection<?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "PENDING_SYNC")).clear();
    }
}
