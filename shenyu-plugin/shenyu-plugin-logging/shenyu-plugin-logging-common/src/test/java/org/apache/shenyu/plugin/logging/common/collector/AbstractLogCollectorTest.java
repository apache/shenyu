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

package org.apache.shenyu.plugin.logging.common.collector;

import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.desensitize.api.enums.DataDesensitizeEnum;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingDeque;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

/**
 * The Test Case For AbstractLogCollector.
 */
public class AbstractLogCollectorTest {

    private final AbstractLogConsumeClient<?, ShenyuRequestLog> logConsumeClient = mock(AbstractLogConsumeClient.class);

    private final AbstractLogCollector<AbstractLogConsumeClient<?, ShenyuRequestLog>, ShenyuRequestLog, GenericGlobalConfig> collector =
            new AbstractLogCollector<>() {
                @Override
                protected AbstractLogConsumeClient<?, ShenyuRequestLog> getLogConsumeClient() {
                    return logConsumeClient;
                }

                @Override
                protected GenericGlobalConfig getLogCollectConfig() {
                    return null;
                }

                @Override
                protected void desensitizeLog(final ShenyuRequestLog log, final KeyWordMatch keyWordMatch, final String desensitizeAlg) {
                }
            };

    @Test
    public void testCollectAddsLogWhenBufferQueueHasCapacity() throws Exception {
        BlockingQueue<ShenyuRequestLog> bufferQueue = new LinkedBlockingDeque<>(1);
        setField(collector, "bufferSize", 1);
        setField(collector, "bufferQueue", bufferQueue);
        ShenyuRequestLog log = new ShenyuRequestLog();

        collector.collect(log);

        assertSame(log, bufferQueue.peek());
    }

    @Test
    public void testCollectDoesNotThrowWhenBufferQueueIsFull() throws Exception {
        ShenyuRequestLog bufferedLog = new ShenyuRequestLog();
        BlockingQueue<ShenyuRequestLog> bufferQueue = new StaleSizeLinkedBlockingDeque();
        bufferQueue.add(bufferedLog);
        setField(collector, "bufferSize", 1);
        setField(collector, "bufferQueue", bufferQueue);

        assertDoesNotThrow(() -> collector.collect(new ShenyuRequestLog()));
        assertSame(bufferedLog, bufferQueue.peek());
    }

    @Test
    public void testCollectDoesNotThrowWhenMultiClientBufferQueueIsFull() throws Exception {
        AbstractLogCollector<AbstractLogConsumeClient<?, ShenyuRequestLog>, ShenyuRequestLog, GenericGlobalConfig> multiClientCollector =
                new AbstractLogCollector<>() {
                    @Override
                    protected AbstractLogConsumeClient<?, ShenyuRequestLog> getLogConsumeClient() {
                        return logConsumeClient;
                    }

                    @Override
                    protected boolean getMultiClient() {
                        return true;
                    }

                    @Override
                    protected GenericGlobalConfig getLogCollectConfig() {
                        return null;
                    }

                    @Override
                    protected void desensitizeLog(final ShenyuRequestLog log, final KeyWordMatch keyWordMatch, final String desensitizeAlg) {
                    }
                };
        ShenyuRequestLog bufferedLog = new ShenyuRequestLog();
        BlockingQueue<ShenyuRequestLog> bufferQueue = new StaleSizeLinkedBlockingDeque();
        bufferQueue.add(bufferedLog);
        setField(multiClientCollector, "bufferSize", 1);
        getBufferQueues(multiClientCollector).put("selector", bufferQueue);
        ShenyuRequestLog log = new ShenyuRequestLog();
        log.setSelectorId("selector");

        assertDoesNotThrow(() -> multiClientCollector.collect(log));
        assertSame(bufferedLog, bufferQueue.peek());
    }

    @Test
    public void testDesensitizeToleratesNullBoxedNumericFields() {
        // a chunked byte-type response reaches desensitize with responseContentLength,
        // status and upstreamResponseTime unset (LoggingServerHttpResponse passes a null
        // writer for byte media and only sets status once the status code is committed)
        ShenyuRequestLog log = new ShenyuRequestLog();
        log.setClientIp("192.168.1.1");
        KeyWordMatch keyWordMatch = new KeyWordMatch(new HashSet<>(Collections.singletonList(GenericLoggingConstant.CLIENT_IP)));
        assertDoesNotThrow(() -> collector.desensitize(log, keyWordMatch, DataDesensitizeEnum.CHARACTER_REPLACE.getDataDesensitizeAlg()));
        assertNull(log.getResponseContentLength());
        assertNull(log.getStatus());
        assertNull(log.getUpstreamResponseTime());
        assertNotEquals("192.168.1.1", log.getClientIp());
    }

    @Test
    public void testDesensitizePreservesPopulatedNumericFields() {
        ShenyuRequestLog log = new ShenyuRequestLog();
        log.setClientIp("192.168.1.1");
        log.setResponseContentLength(1024);
        log.setStatus(200);
        log.setUpstreamResponseTime(15L);
        Set<String> keyWords = new HashSet<>(Collections.singletonList(GenericLoggingConstant.CLIENT_IP));
        collector.desensitize(log, new KeyWordMatch(keyWords), DataDesensitizeEnum.CHARACTER_REPLACE.getDataDesensitizeAlg());
        assertEquals(1024, log.getResponseContentLength());
        assertEquals(200, log.getStatus());
        assertEquals(15L, log.getUpstreamResponseTime());
    }

    private static void setField(final Object target, final String fieldName, final Object value) throws Exception {
        Field field = AbstractLogCollector.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(target, value);
    }

    @SuppressWarnings("unchecked")
    private static Map<String, BlockingQueue<ShenyuRequestLog>> getBufferQueues(final Object target) throws Exception {
        Field field = AbstractLogCollector.class.getDeclaredField("bufferQueueS");
        field.setAccessible(true);
        return (Map<String, BlockingQueue<ShenyuRequestLog>>) field.get(target);
    }

    private static final class StaleSizeLinkedBlockingDeque extends LinkedBlockingDeque<ShenyuRequestLog> {

        private StaleSizeLinkedBlockingDeque() {
            super(1);
        }

        @Override
        public int size() {
            return 0;
        }
    }
}
