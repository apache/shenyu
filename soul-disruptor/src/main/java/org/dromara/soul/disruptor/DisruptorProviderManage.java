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

package org.dromara.soul.disruptor;

import com.lmax.disruptor.BlockingWaitStrategy;
import com.lmax.disruptor.IgnoreExceptionHandler;
import com.lmax.disruptor.RingBuffer;
import com.lmax.disruptor.dsl.Disruptor;
import com.lmax.disruptor.dsl.ProducerType;
import org.dromara.soul.disruptor.event.DataEvent;
import org.dromara.soul.disruptor.thread.DisruptorThreadFactory;

/**
 * DisruptorProviderManage.
 * disruptor provider manager.
 *
 * @param <T> the type parameter
 * @author xiaoyu sixh
 */
public class DisruptorProviderManage<T> {

    public static final Integer DEFAULT_SIZE = 4096 << 1 << 1;

    private static final Integer DEFAULT_CONSUMER_SIZE = Runtime.getRuntime().availableProcessors() << 1;

    private final Integer size;

    private DisruptorProvider<T> provider;

    private Integer consumerSize;

    private DisruptorConsumerFactory<T> consumerFactory;

    /**
     * Instantiates a new Disruptor provider manage.
     *
     * @param consumerFactory the consumer factory
     * @param ringBufferSize  the size
     */
    public DisruptorProviderManage(final DisruptorConsumerFactory<T> consumerFactory, final Integer ringBufferSize) {
        this(consumerFactory,
                DEFAULT_CONSUMER_SIZE,
                ringBufferSize);
    }

    /**
     * Instantiates a new Disruptor provider manage.
     *
     * @param consumerFactory the consumer factory
     */
    public DisruptorProviderManage(final DisruptorConsumerFactory<T> consumerFactory) {
        this(consumerFactory, DEFAULT_CONSUMER_SIZE, DEFAULT_SIZE);
    }

    /**
     * Instantiates a new Disruptor provider manage.
     *
     * @param consumerFactory the consumer factory
     * @param consumerSize    the consumer size
     * @param ringBufferSize  the ringBuffer size
     */
    public DisruptorProviderManage(final DisruptorConsumerFactory<T> consumerFactory,
                                   final int consumerSize,
                                   final int ringBufferSize) {
        this.consumerFactory = consumerFactory;
        this.size = ringBufferSize;
        this.consumerSize = consumerSize;

    }

    /**
     * start disruptor.
     */
    @SuppressWarnings("unchecked")
    public void startup() {
        Disruptor<DataEvent<T>> disruptor = new Disruptor<>(new DisruptorEventFactory<>(),
                size,
                DisruptorThreadFactory.create("disruptor_consumer_" + consumerFactory.fixName(), false),
                ProducerType.MULTI,
                new BlockingWaitStrategy());
        DisruptorConsumer<T>[] consumers = new DisruptorConsumer[consumerSize];
        for (int i = 0; i < consumerSize; i++) {
            consumers[i] = new DisruptorConsumer<>(consumerFactory);
        }
        disruptor.handleEventsWithWorkerPool(consumers);
        disruptor.setDefaultExceptionHandler(new IgnoreExceptionHandler());
        disruptor.start();
        RingBuffer<DataEvent<T>> ringBuffer = disruptor.getRingBuffer();
        provider = new DisruptorProvider<>(ringBuffer, disruptor);
    }


    /**
     * Gets provider.
     *
     * @return the provider
     */
    public DisruptorProvider<T> getProvider() {
        return provider;
    }
}
