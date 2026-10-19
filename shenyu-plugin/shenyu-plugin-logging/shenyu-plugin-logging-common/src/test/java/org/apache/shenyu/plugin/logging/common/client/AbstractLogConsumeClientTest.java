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

package org.apache.shenyu.plugin.logging.common.client;

import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for {@link AbstractLogConsumeClient}.
 */
public final class AbstractLogConsumeClientTest {

    @Test
    public void testConsumeIsSkippedWhenInitializationFails() throws Exception {
        TestLogConsumeClient client = new TestLogConsumeClient(false);

        client.initClient(new GenericGlobalConfig());
        client.consume(Collections.singletonList(new ShenyuRequestLog()));

        assertEquals(0, client.getConsumeCount());
        assertEquals(0, client.getCloseCount());
    }

    @Test
    public void testConsumeRunsWhenInitializationSucceeds() throws Exception {
        TestLogConsumeClient client = new TestLogConsumeClient(true);

        try {
            client.initClient(new GenericGlobalConfig());
            client.consume(Collections.singletonList(new ShenyuRequestLog()));

            assertEquals(1, client.getConsumeCount());
        } finally {
            client.close();
        }
        assertEquals(1, client.getCloseCount());
    }

    @Test
    public void testClose0StillRunsWhenHookRemovalFails() {
        ThrowingHookLogConsumeClient client = new ThrowingHookLogConsumeClient();

        try {
            client.initClient(new GenericGlobalConfig());
            client.close();
        } finally {
            Runtime.getRuntime().removeShutdownHook(client.getRegisteredHook());
        }

        assertEquals(1, client.getCloseCount());
    }

    @Test
    public void testCloseIsIdempotent() {
        TestLogConsumeClient client = new TestLogConsumeClient(true);

        try {
            client.initClient(new GenericGlobalConfig());
            client.close();
            client.close();
        } finally {
            // hook already removed by the first close(); removal is a no-op now
            client.close();
        }

        assertEquals(1, client.getCloseCount());
    }

    @Test
    public void testFailedReinitializationLeavesClientStopped() throws Exception {
        TestLogConsumeClient client = new TestLogConsumeClient(true);
        client.initClient(new GenericGlobalConfig());
        client.setInitializationResult(false);

        client.initClient(new GenericGlobalConfig());
        client.consume(Collections.singletonList(new ShenyuRequestLog()));

        assertEquals(0, client.getConsumeCount());
        assertEquals(1, client.getCloseCount());
    }

    private static final class TestLogConsumeClient extends AbstractLogConsumeClient<GenericGlobalConfig, ShenyuRequestLog> {

        private boolean initializationResult;

        private int consumeCount;

        private int closeCount;

        private TestLogConsumeClient(final boolean initializationResult) {
            this.initializationResult = initializationResult;
        }

        @Override
        public boolean initClient0(final GenericGlobalConfig config) {
            return initializationResult;
        }

        @Override
        public void consume0(final List<ShenyuRequestLog> logs) {
            consumeCount++;
        }

        @Override
        public void close0() {
            closeCount++;
        }

        private void setInitializationResult(final boolean initializationResult) {
            this.initializationResult = initializationResult;
        }

        private int getConsumeCount() {
            return consumeCount;
        }

        private int getCloseCount() {
            return closeCount;
        }
    }

    /**
     * Simulates the JVM-shutdown scenario where {@code removeShutdownHook} throws
     * {@link IllegalStateException}: cleanup must still reach {@code close0()}.
     */
    private static final class ThrowingHookLogConsumeClient extends AbstractLogConsumeClient<GenericGlobalConfig, ShenyuRequestLog> {

        private final AtomicReference<Thread> registeredHook = new AtomicReference<>();

        private int closeCount;

        @Override
        public boolean initClient0(final GenericGlobalConfig config) {
            registeredHook.set(new Thread(this::close));
            return true;
        }

        @Override
        public void consume0(final List<ShenyuRequestLog> logs) {
            // not used in this test
        }

        @Override
        public void close0() {
            closeCount++;
        }

        @Override
        protected void removeHook(final Thread hook) {
            throw new IllegalStateException("Shutdown in progress");
        }

        private Thread getRegisteredHook() {
            return registeredHook.get();
        }

        private int getCloseCount() {
            return closeCount;
        }
    }
}
