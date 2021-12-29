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

package org.apache.shenyu.disruptor.consumer;

import com.lmax.disruptor.WorkHandler;
import org.apache.shenyu.disruptor.event.DataEvent;
import org.apache.shenyu.disruptor.event.OrderlyDataEvent;
import org.apache.shenyu.disruptor.thread.OrderlyExecutor;

import java.util.concurrent.ThreadPoolExecutor;

/**
 * The type Queue consumer.
 *
 * @param <T> the type parameter
 */
public class QueueConsumer<T> implements WorkHandler<DataEvent<T>> {
    
    private final OrderlyExecutor executor;
    
    private final QueueConsumerFactory<T> factory;
    
    /**
     * Instantiates a new Queue consumer.
     *
     * @param executor the executor
     * @param factory  the factory
     */
    public QueueConsumer(final OrderlyExecutor executor, final QueueConsumerFactory<T> factory) {
        this.executor = executor;
        this.factory = factory;
    }
    
    @Override
    public void onEvent(final DataEvent<T> t) {
        if (t != null) {
            ThreadPoolExecutor executor = orderly(t);
            QueueConsumerExecutor<T> queueConsumerExecutor = factory.create();
            queueConsumerExecutor.setData(t.getData());
            // help gc
            t.setData(null);
            executor.execute(queueConsumerExecutor);
        }
    }
    
    private ThreadPoolExecutor orderly(final DataEvent<T> t) {
        if (t instanceof OrderlyDataEvent && !isEmpty(((OrderlyDataEvent<T>) t).getHash())) {
            return executor.select(((OrderlyDataEvent<T>) t).getHash());
        } else {
            return executor;
        }
    }
    
    private boolean isEmpty(final String t) {
        return t == null || t.isEmpty();
    }
}
