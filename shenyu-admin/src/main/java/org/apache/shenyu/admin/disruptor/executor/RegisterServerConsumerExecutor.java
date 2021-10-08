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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.disruptor.consumer.QueueConsumerExecutor;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.subsriber.ExecutorSubscriber;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
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
        results = results.stream().filter(data -> isValidData(data)).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(results)) {
            return;
        }
        getType(results).executor(results);
    }

    private boolean isValidData(final Object data) {
        if (data instanceof URIRegisterDTO) {
            URIRegisterDTO uriRegisterDTO = (URIRegisterDTO) data;
            return Objects.nonNull(uriRegisterDTO.getPort())
                    && StringUtils.isNoneBlank(uriRegisterDTO.getAppName(), uriRegisterDTO.getHost());
        }
        if (data instanceof MetaDataRegisterDTO) {
            MetaDataRegisterDTO metaDataRegisterDTO = (MetaDataRegisterDTO) data;
            return Objects.nonNull(metaDataRegisterDTO.getPort())
                    && StringUtils.isNoneBlank(metaDataRegisterDTO.getAppName(),
                    metaDataRegisterDTO.getHost(),
                    metaDataRegisterDTO.getPath(),
                    metaDataRegisterDTO.getRuleName(),
                    metaDataRegisterDTO.getRpcType());
        }
        return true;
    }
    
    private ExecutorSubscriber getType(final List<DataTypeParent> list) {
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
