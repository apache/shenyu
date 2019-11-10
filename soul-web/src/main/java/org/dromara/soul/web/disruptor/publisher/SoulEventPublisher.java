/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.disruptor.publisher;

import com.lmax.disruptor.BlockingWaitStrategy;
import com.lmax.disruptor.IgnoreExceptionHandler;
import com.lmax.disruptor.RingBuffer;
import com.lmax.disruptor.dsl.Disruptor;
import com.lmax.disruptor.dsl.ProducerType;
import org.dromara.soul.common.concurrent.SoulThreadFactory;
import org.dromara.soul.web.disruptor.event.SoulDataEvent;
import org.dromara.soul.web.disruptor.factory.SoulEventFactory;
import org.dromara.soul.web.disruptor.handler.SoulDataHandler;
import org.dromara.soul.web.disruptor.translator.SoulEventTranslator;
import org.dromara.soul.web.influxdb.entity.MonitorDO;
import org.dromara.soul.web.influxdb.service.InfluxDbService;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Value;

import java.util.concurrent.Executor;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * disruptor start and publishEvent.
 *
 * @author xiaoyu(Myth)
 */
public class SoulEventPublisher implements InitializingBean, DisposableBean {

    private Disruptor<SoulDataEvent> disruptor;

    private final InfluxDbService influxDbService;

    @Value("${soul.disruptor.bufferSize:4096}")
    private int bufferSize;

    @Value("${soul.disruptor.threadSize:16}")
    private int threadSize;

    @Value("${soul.disruptor.taskQueueSize:4096}")
    private int threadPoolQueueSize;

    /**
     * Instantiates a new Soul event publisher.
     *
     * @param influxDbService the influx db service
     */
    public SoulEventPublisher(final InfluxDbService influxDbService) {
        this.influxDbService = influxDbService;
    }

    /**
     * disruptor start with bufferSize.
     */
    private void start() {
        disruptor = new Disruptor<>(new SoulEventFactory(), bufferSize,
                SoulThreadFactory.create("monitor-disruptor-thread-", false),
                ProducerType.MULTI,
                new BlockingWaitStrategy());

        final Executor executor = new ThreadPoolExecutor(threadSize, threadSize, 0, TimeUnit.MILLISECONDS,
                new LinkedBlockingQueue<>(threadPoolQueueSize),
                SoulThreadFactory.create("monitor-disruptor-executor", false),
                new ThreadPoolExecutor.AbortPolicy());

        SoulDataHandler[] consumers = new SoulDataHandler[threadSize];
        for (int i = 0; i < threadSize; i++) {
            consumers[i] = new SoulDataHandler(executor, influxDbService);
        }
        disruptor.handleEventsWithWorkerPool(consumers);
        disruptor.setDefaultExceptionHandler(new IgnoreExceptionHandler());
        disruptor.start();
    }

    /**
     * publish disruptor event.
     *
     * @param monitorDO data.
     */
    public void publishEvent(final MonitorDO monitorDO) {
        final RingBuffer<SoulDataEvent> ringBuffer = disruptor.getRingBuffer();
        ringBuffer.publishEvent(new SoulEventTranslator(), monitorDO);

    }

    @Override
    public void destroy() {
        disruptor.shutdown();
    }

    @Override
    public void afterPropertiesSet() {
        start();
    }
}
