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
import org.apache.shenyu.register.common.subsriber.ExecutorSubscriber;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The type Consumer executor.
 */
@SuppressWarnings("all")
public final class RegisterClientConsumerExecutor extends QueueConsumerExecutor<DataTypeParent> {
    
    private Map<DataType, ExecutorSubscriber> subscribers = new HashMap<>();
    
    private RegisterClientConsumerExecutor(final Map<DataType, ExecutorTypeSubscriber> executorSubscriberMap) {
        this.subscribers.putAll(executorSubscriberMap);
    }

    @Override
    public void run() {
        DataTypeParent dataTypeParent = getData();
        subscribers.get(dataTypeParent.getType()).executor(Lists.newArrayList(dataTypeParent));
    }
    
    /**
     * The type Register client executor factory.
     */
    public static class RegisterClientExecutorFactory extends AbstractQueueConsumerFactory {
        
        @Override
        public QueueConsumerExecutor create() {
            Map<DataType, ExecutorTypeSubscriber> maps = getSubscribers().stream().map(e -> (ExecutorTypeSubscriber) e).collect(Collectors.toMap(ExecutorTypeSubscriber::getType, e -> e));
            return new RegisterClientConsumerExecutor(maps);
        }

        @Override
        public String fixName() {
            return "shenyu_register_client";
        }
    }
}
