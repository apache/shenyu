/*
 *
 *  * Licensed to the Apache Software Foundation (ASF) under one or more
 *  * contributor license agreements.  See the NOTICE file distributed with
 *  * this work for additional information regarding copyright ownership.
 *  * The ASF licenses this file to You under the Apache License, Version 2.0
 *  * (the "License"); you may not use this file except in compliance with
 *  * the License.  You may obtain a copy of the License at
 *  *
 *  *     http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *
 */

package org.dromara.soul.web.disruptor.publisher;

import com.lmax.disruptor.RingBuffer;
import com.lmax.disruptor.SleepingWaitStrategy;
import com.lmax.disruptor.dsl.Disruptor;
import com.lmax.disruptor.dsl.ProducerType;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.web.concurrent.SoulThreadFactory;
import org.dromara.soul.web.disruptor.event.SoulDataEvent;
import org.dromara.soul.web.disruptor.factory.SoulEventFactory;
import org.dromara.soul.web.disruptor.handler.ClearingEventHandler;
import org.dromara.soul.web.disruptor.handler.SoulEventHandler;
import org.dromara.soul.web.disruptor.translator.SoulEventTranslator;
import org.dromara.soul.web.influxdb.entity.MonitorDO;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * disruptor start and publishEvent.
 * @author xiaoyu(Myth)
 */
@Component
public class SoulEventPublisher implements InitializingBean, DisposableBean {

    private Disruptor<SoulDataEvent> disruptor;

    private final SoulEventHandler soulEventHandler;

    private final ClearingEventHandler clearingEventHandler;

    @Value("${soul.bufferSize}")
    private int bufferSize;

    @Autowired
    public SoulEventPublisher(final SoulEventHandler soulEventHandler,
                              final ClearingEventHandler clearingEventHandler) {
        this.soulEventHandler = soulEventHandler;
        this.clearingEventHandler = clearingEventHandler;
    }

    /**
     * disruptor start with bufferSize.
     *
     * @param bufferSize bufferSize
     */
    private void start(final int bufferSize) {
        disruptor = new Disruptor<>(new SoulEventFactory(),
                bufferSize, SoulThreadFactory.create(Constants.SOUL_DISRUPTOR_THREAD_NAME,
                false), ProducerType.SINGLE, new SleepingWaitStrategy());
        disruptor.handleEventsWith(soulEventHandler).then(clearingEventHandler);
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
        start(bufferSize);
    }
}
