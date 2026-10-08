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

package org.apache.shenyu.plugin.logging.common.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.collector.AbstractLogCollector;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Tests the shared logging handler lifecycle with an in-memory consume client.
 */
public final class AbstractLogPluginDataHandlerTest {

    private static final String PLUGIN_ID = "logging-lifecycle-test";

    private final PluginData pluginData = PluginData.builder().id(PLUGIN_ID).enabled(true).config("{\"sampleRate\":\"1\"}").build();

    private final CountDownLatch consumed = new CountDownLatch(1);

    private int initializationCount;

    private int clientCloseCount;

    private int collectorStartCount;

    private int collectorCloseCount;

    private ShenyuRequestLog consumedLog;

    private final AbstractLogConsumeClient<TestLogConfig, ShenyuRequestLog> client = new AbstractLogConsumeClient<>() {
        @Override
        public boolean initClient0(final TestLogConfig config) {
            initializationCount++;
            return true;
        }

        @Override
        public void consume0(final List<ShenyuRequestLog> logs) {
            consumedLog = logs.get(0);
            consumed.countDown();
        }

        @Override
        public void close0() {
            clientCloseCount++;
        }
    };

    private final AbstractLogCollector<AbstractLogConsumeClient<TestLogConfig, ShenyuRequestLog>, ShenyuRequestLog, TestLogConfig> collector = new AbstractLogCollector<>() {
        @Override
        public synchronized void start() {
            collectorStartCount++;
            super.start();
        }

        @Override
        public synchronized void close() throws Exception {
            collectorCloseCount++;
            super.close();
        }

        @Override
        protected AbstractLogConsumeClient<TestLogConfig, ShenyuRequestLog> getLogConsumeClient() {
            return client;
        }

        @Override
        protected TestLogConfig getLogCollectConfig() {
            return new TestLogConfig();
        }

        @Override
        protected void desensitizeLog(final ShenyuRequestLog log, final KeyWordMatch keyWordMatch, final String desensitizeAlg) {
        }
    };

    private final TestPluginDataHandler handler = new TestPluginDataHandler();

    @BeforeEach
    public void setUp() {
        TestLogConfig staleConfig = new TestLogConfig();
        staleConfig.setSampleRate("0.5");
        Singleton.INST.single(TestLogConfig.class, staleConfig);
    }

    @AfterEach
    public void tearDown() throws Exception {
        try {
            collector.close();
        } finally {
            try {
                AbstractLogPluginDataHandler.getPluginGlobalConfigMap().remove(PLUGIN_ID);
            } finally {
                Singleton.INST.remove(TestLogConfig.class);
            }
        }
    }

    @Test
    public void testReenableUnchangedConfigRestartsCollectorAndClient() throws Exception {
        handler.handlerPlugin(pluginData);
        assertEquals(1, initializationCount);
        assertEquals(1, collectorStartCount);
        final TestLogConfig retainedConfig = Singleton.INST.get(TestLogConfig.class);

        pluginData.setEnabled(false);
        handler.handlerPlugin(pluginData);
        assertEquals(1, collectorCloseCount);
        assertEquals(1, clientCloseCount);
        assertSame(retainedConfig, Singleton.INST.get(TestLogConfig.class));

        pluginData.setEnabled(true);
        handler.handlerPlugin(pluginData);
        assertEquals(2, initializationCount);
        assertEquals(2, collectorStartCount);
        handler.handlerPlugin(pluginData);
        assertEquals(2, initializationCount);
        assertEquals(2, collectorStartCount);

        ShenyuRequestLog log = new ShenyuRequestLog();
        collector.collect(log);
        assertTrue(consumed.await(5, TimeUnit.SECONDS));
        assertSame(log, consumedLog);
    }

    @Test
    public void testReenableUnchangedConfigAfterCloseFailure() throws Exception {
        LogCollector<ShenyuRequestLog> failingCollector = mock(LogCollector.class);
        TestPluginDataHandler failingHandler = spy(handler);
        doReturn(failingCollector).when(failingHandler).logCollector();
        doThrow(new Exception("Failed to close collector")).when(failingCollector).close();
        failingHandler.handlerPlugin(pluginData);

        pluginData.setEnabled(false);
        assertDoesNotThrow(() -> failingHandler.handlerPlugin(pluginData));
        pluginData.setEnabled(true);
        failingHandler.handlerPlugin(pluginData);
        assertEquals(2, initializationCount);
        verify(failingCollector, times(2)).start();
        verify(failingCollector, times(1)).close();
    }

    private static final class TestLogConfig extends GenericGlobalConfig {
        @Override
        public boolean equals(final Object other) {
            return other instanceof TestLogConfig
                    && Objects.equals(getSampleRate(), ((TestLogConfig) other).getSampleRate());
        }

        @Override
        public int hashCode() {
            return Objects.hash(getSampleRate());
        }
    }

    private final class TestPluginDataHandler extends AbstractLogPluginDataHandler<TestLogConfig, GenericApiConfig> {
        @Override
        public String pluginNamed() {
            return "loggingLifecycleTest";
        }

        @Override
        protected LogCollector logCollector() {
            return collector;
        }

        @Override
        protected void doRefreshConfig(final TestLogConfig globalLogConfig) {
            client.initClient(globalLogConfig);
        }
    }
}
