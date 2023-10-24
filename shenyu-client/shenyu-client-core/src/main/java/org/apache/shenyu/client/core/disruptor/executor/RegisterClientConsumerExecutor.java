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

package org.apache.shenyu.client.core.disruptor.executor;

import com.google.common.collect.Lists;
import org.apache.shenyu.disruptor.consumer.QueueConsumerExecutor;
import org.apache.shenyu.register.common.subsriber.AbstractQueueConsumerFactory;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.EnumMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The type Consumer executor.
 */
public final class RegisterClientConsumerExecutor<T extends DataTypeParent> extends QueueConsumerExecutor<T> {
    
    private final Map<DataType, ExecutorTypeSubscriber<T>> subscribers;
    
    private RegisterClientConsumerExecutor(final Map<DataType, ExecutorTypeSubscriber<T>> executorSubscriberMap) {
        this.subscribers = new EnumMap<>(executorSubscriberMap);
    }

    @Override
    public void run() {
        final T data = getData();
        subscribers.get(data.getType()).executor(Lists.newArrayList(data));
    }
    
    /**
     * The type Register client executor factory.
     */
    public static class RegisterClientExecutorFactory<T extends DataTypeParent> extends AbstractQueueConsumerFactory<T> {
        
        @Override
        public RegisterClientConsumerExecutor<T> create() {
            Map<DataType, ExecutorTypeSubscriber<T>> map = getSubscribers()
                    .stream()
                    .map(e -> (ExecutorTypeSubscriber<T>) e)
                    .collect(Collectors.toMap(ExecutorTypeSubscriber::getType, e -> e));
            return new RegisterClientConsumerExecutor<>(map);
        }

        @Override
        public String fixName() {
            return "shenyu_register_client";
        }
    }
}
