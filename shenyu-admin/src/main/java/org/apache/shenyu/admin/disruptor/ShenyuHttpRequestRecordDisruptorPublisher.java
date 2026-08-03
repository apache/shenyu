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

package org.apache.shenyu.admin.disruptor;

import org.apache.shenyu.admin.config.properties.HttpRecordProperties;
import org.apache.shenyu.admin.disruptor.executor.ShenyuHttpRequestRecordConsumerExecutor.ShenyuHttpRequestRecordConsumerExecutorFactory;
import org.apache.shenyu.admin.model.dto.ShenyuHttpRequestRecordDTO;
import org.apache.shenyu.admin.record.HttpRecordRepository;
import org.apache.shenyu.disruptor.DisruptorProviderManage;
import org.apache.shenyu.disruptor.provider.DisruptorProvider;

import java.util.Map;

public class ShenyuHttpRequestRecordDisruptorPublisher {

    private DisruptorProviderManage<ShenyuHttpRequestRecordDTO> providerManage;


    public void start(final HttpRecordProperties properties, final Map<String, HttpRecordRepository> repositoryMap) {
        ShenyuHttpRequestRecordConsumerExecutorFactory factory = new ShenyuHttpRequestRecordConsumerExecutorFactory(properties, repositoryMap);
        providerManage = new DisruptorProviderManage<>(factory);
        providerManage.startup();
    }

    public void publish(final ShenyuHttpRequestRecordDTO dto) {
        DisruptorProvider<ShenyuHttpRequestRecordDTO> provider = providerManage.getProvider();
        provider.onData(dto);
    }


}
