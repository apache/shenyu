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

package org.apache.shenyu.plugin.record.collector;

import org.apache.shenyu.plugin.record.config.HttpRecordCollectConfig;
import org.apache.shenyu.plugin.record.entity.ShenyuHttpRequestRecord;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

public final class HttpRecordCollectorTest {

    private HttpRecordCollector collector;

    @BeforeEach
    public void setUp() throws Exception {
        collector = HttpRecordCollector.INSTANCE;
        HttpRecordCollectConfig.INSTANCE.setRecordConfig(new HttpRecordCollectConfig.RecordConfig());
    }

    @AfterEach
    public void tearDown() throws Exception {
        setField(collector, "bufferQueue", null);
        AtomicInteger bufferSize = getField(collector, "bufferSize");
        bufferSize.set(0);
        setField(collector, "maxBufferSize", 0);
    }

    @Test
    public void testGetInstance() {
        HttpRecordCollector instance1 = HttpRecordCollector.getInstance();
        HttpRecordCollector instance2 = HttpRecordCollector.getInstance();
        assertNotNull(instance1);
        assertSame(instance1, instance2);
    }

    @Test
    public void testCollectWithNullRecord() {
        collector.collect(null);
    }

    @Test
    public void testCollectWithNullBufferQueue() {
        ShenyuHttpRequestRecord record = new ShenyuHttpRequestRecord();
        record.setTraceId("test-trace-id");
        collector.collect(record);
    }

    @Test
    public void testCollectPutsRecordIntoQueue() throws Exception {
        ConcurrentLinkedQueue<ShenyuHttpRequestRecord> queue = new ConcurrentLinkedQueue<>();
        setField(collector, "bufferQueue", queue);
        setField(collector, "maxBufferSize", 100);

        ShenyuHttpRequestRecord record = new ShenyuHttpRequestRecord();
        record.setTraceId("test-trace-id");
        record.setRequestUri("/test");
        record.setMethod("GET");
        collector.collect(record);

        assertEquals(1, queue.size());
        assertEquals("test-trace-id", queue.peek().getTraceId());
    }

    @Test
    public void testCollectDropsRecordWhenBufferFull() throws Exception {
        ConcurrentLinkedQueue<ShenyuHttpRequestRecord> queue = new ConcurrentLinkedQueue<>();
        setField(collector, "bufferQueue", queue);
        setField(collector, "maxBufferSize", 1);

        ShenyuHttpRequestRecord record1 = new ShenyuHttpRequestRecord();
        record1.setTraceId("first");
        collector.collect(record1);

        ShenyuHttpRequestRecord record2 = new ShenyuHttpRequestRecord();
        record2.setTraceId("dropped");
        collector.collect(record2);

        assertEquals(1, queue.size());
        assertEquals("first", queue.peek().getTraceId());
    }

    @Test
    public void testStopWithoutStart() {
        collector.stop();
    }


    @Test
    public void testBatchSizeDefault() {
        HttpRecordCollectConfig.RecordConfig config = new HttpRecordCollectConfig.RecordConfig();
        assertEquals(100, config.getBatchSize());
    }

    @Test
    public void testBatchIntervalDefault() {
        HttpRecordCollectConfig.RecordConfig config = new HttpRecordCollectConfig.RecordConfig();
        assertEquals(5000L, config.getBatchIntervalMs());
    }

    @Test
    public void testMaxBodySizeDefault() {
        HttpRecordCollectConfig.RecordConfig config = new HttpRecordCollectConfig.RecordConfig();
        assertEquals(524288, config.getMaxBodySize());
    }

    @SuppressWarnings("unchecked")
    private static <T> T getField(final Object target, final String fieldName) throws Exception {
        Field field = target.getClass().getDeclaredField(fieldName);
        field.setAccessible(true);
        return (T) field.get(target);
    }

    private static void setField(final Object target, final String fieldName, final Object value) throws Exception {
        Field field = target.getClass().getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(target, value);
    }
}