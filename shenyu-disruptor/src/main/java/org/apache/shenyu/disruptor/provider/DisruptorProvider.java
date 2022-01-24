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

package org.apache.shenyu.disruptor.provider;

import com.lmax.disruptor.EventTranslatorOneArg;
import com.lmax.disruptor.EventTranslatorTwoArg;
import com.lmax.disruptor.RingBuffer;
import com.lmax.disruptor.dsl.Disruptor;
import org.apache.shenyu.disruptor.event.DataEvent;
import org.apache.shenyu.disruptor.event.OrderlyDataEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * DisruptorProvider.
 * disruptor provider definition.
 *
 * @param <T> the type parameter
 */
public class DisruptorProvider<T> {
    
    private final RingBuffer<DataEvent<T>> ringBuffer;
    
    private final Disruptor<DataEvent<T>> disruptor;
    
    private final boolean isOrderly;
    
    private final EventTranslatorOneArg<DataEvent<T>, T> translatorOneArg = (event, sequence, t) -> event.setData(t);
    
    private final EventTranslatorTwoArg<DataEvent<T>, T, String> orderlyArg = (event, sequence, t, orderly) -> {
        if (event instanceof OrderlyDataEvent) {
            ((OrderlyDataEvent<T>) event).setHash(orderly);
        }
        event.setData(t);
    };
    
    /**
     * The Logger.
     */
    private final Logger logger = LoggerFactory.getLogger(DisruptorProvider.class);
    
    /**
     * Instantiates a new Disruptor provider.
     *
     * @param ringBuffer the ring buffer
     * @param disruptor  the disruptor
     * @param isOrderly  the orderly Whether to execute sequentially.
     */
    public DisruptorProvider(final RingBuffer<DataEvent<T>> ringBuffer, final Disruptor<DataEvent<T>> disruptor, final boolean isOrderly) {
        this.ringBuffer = ringBuffer;
        this.disruptor = disruptor;
        this.isOrderly = isOrderly;
    }

    /**
     * Send a data.
     *
     * @param data the data
     */
    public void onData(final T data) {
        if (isOrderly) {
            throw new IllegalArgumentException("The current provider is  of orderly type. Please use onOrderlyData() method.");
        }
        try {
            ringBuffer.publishEvent(translatorOneArg, data);
        } catch (Exception ex) {
            logger.error("ex", ex);
        }
    }
    
    /**
     * On orderly data.
     *
     * @param data      the data
     * @param hashArray the hashArray.
     */
    public void onOrderlyData(final T data, final String... hashArray) {
        if (!this.isOrderly) {
            throw new IllegalArgumentException("The current provider is not of orderly type. Please use onData() method.");
        }
        try {
            String hash = String.join(":", hashArray);
            ringBuffer.publishEvent(orderlyArg, data, hash);
        } catch (Exception ex) {
            logger.error("ex", ex);
        }
    }
    
    /**
     * Shutdown.
     */
    public void shutdown() {
        if (null != disruptor) {
            disruptor.shutdown();
        }
    }
}
