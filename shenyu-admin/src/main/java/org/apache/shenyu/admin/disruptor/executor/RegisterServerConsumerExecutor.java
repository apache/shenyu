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

package org.apache.shenyu.admin.disruptor.executor;

import org.apache.shenyu.register.common.subsriber.ExecutorSubscriber;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.disruptor.consumer.QueueConsumerExecutor;
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The type Consumer executor.
 */
@SuppressWarnings("all")
public final class RegisterServerConsumerExecutor extends QueueConsumerExecutor<List<DataTypeParent>> {
    
    private Map<DataType, ExecutorSubscriber> subscribers = new HashMap<>();

    private RegisterServerConsumerExecutor(final Map<DataType, ExecutorTypeSubscriber> executorSubscriberMap) {
        this.subscribers.putAll(executorSubscriberMap);
    }

    @Override
    public void run() {
        List<DataTypeParent> results = getData();
        getType(results).executor(results);
    }
    
    private ExecutorSubscriber getType(final List<DataTypeParent> list) {
        if (list == null || list.isEmpty()) {
            return null;
        }
        DataTypeParent result = list.get(0);
        return subscribers.get(result.getType());
    }
    
    public static class RegisterServerExecutorFactory extends AbstractQueueConsumerFactory {

        @Override
        public QueueConsumerExecutor create() {
            Map<DataType, ExecutorTypeSubscriber> maps = getSubscribers().stream().map(e -> (ExecutorTypeSubscriber) e).collect(Collectors.toMap(ExecutorTypeSubscriber::getType, e -> e));
            return new RegisterServerConsumerExecutor(maps);
        }

        @Override
        public String fixName() {
            return "shenyu_register_server";
        }
    }
}
