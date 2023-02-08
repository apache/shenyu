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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.disruptor.consumer.QueueConsumerExecutor;
import org.apache.shenyu.disruptor.consumer.QueueConsumerFactory;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.subsriber.ExecutorSubscriber;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * The type Consumer executor.
 */
public final class RegisterServerConsumerExecutor extends QueueConsumerExecutor<Collection<DataTypeParent>> {
    
    private final Map<DataType, ExecutorSubscriber<DataTypeParent>> subscribers;
    
    private RegisterServerConsumerExecutor(final Map<DataType, ExecutorTypeSubscriber<DataTypeParent>> executorSubscriberMap) {
        this.subscribers = new HashMap<>(executorSubscriberMap);
    }
    
    @Override
    public void run() {
        Collection<DataTypeParent> results = getData()
                .stream()
                .filter(this::isValidData)
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(results)) {
            return;
        }
        selectExecutor(results).executor(results);
    }
    
    private boolean isValidData(final Object data) {
        if (data instanceof URIRegisterDTO) {
            URIRegisterDTO uriRegisterDTO = (URIRegisterDTO) data;
            return StringUtils.isNoneBlank(uriRegisterDTO.getContextPath(), uriRegisterDTO.getRpcType());
        }
        if (data instanceof MetaDataRegisterDTO) {
            MetaDataRegisterDTO metaDataRegisterDTO = (MetaDataRegisterDTO) data;
            return StringUtils.isNoneBlank(metaDataRegisterDTO.getAppName(),
                    metaDataRegisterDTO.getPath(),
                    metaDataRegisterDTO.getRuleName(),
                    metaDataRegisterDTO.getRpcType());
        }
        return true;
    }
    
    private ExecutorSubscriber<DataTypeParent> selectExecutor(final Collection<DataTypeParent> list) {
        final Optional<DataTypeParent> first = list.stream().findFirst();
        return subscribers.get(first.orElseThrow(() -> new RuntimeException("the data type is not found")).getType());
    }
    
    public static class RegisterServerExecutorFactory implements QueueConsumerFactory<Collection<DataTypeParent>> {
    
        /**
         * The Subscribers.
         */
        private final Set<ExecutorTypeSubscriber<? extends DataTypeParent>> subscribers = new HashSet<>();
    
        @Override
        public QueueConsumerExecutor<Collection<DataTypeParent>> create() {
            Map<DataType, ExecutorTypeSubscriber<DataTypeParent>> maps = getSubscribers()
                    .stream()
                    .map(e -> (ExecutorTypeSubscriber<DataTypeParent>) e)
                    .collect(Collectors.toMap(ExecutorTypeSubscriber::getType, Function.identity()));
            return new RegisterServerConsumerExecutor(maps);
        }
    
        @Override
        public String fixName() {
            return "shenyu_register_server";
        }
    
    
        /**
         * Add subscribers abstract queue consumer factory.
         *
         * @param subscriber the subscriber
         * @return the abstract queue consumer factory
         */
        public RegisterServerExecutorFactory addSubscribers(final ExecutorTypeSubscriber<? extends DataTypeParent> subscriber) {
            subscribers.add(subscriber);
            return this;
        }
    
        /**
         * Gets subscribers.
         *
         * @return the subscribers
         */
        public Set<ExecutorTypeSubscriber<? extends DataTypeParent>> getSubscribers() {
            return subscribers;
        }
    }
}
