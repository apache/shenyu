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
import org.apache.shenyu.admin.config.properties.HttpRecordProperties;
import org.apache.shenyu.admin.model.dto.ShenyuHttpRequestRecordDTO;
import org.apache.shenyu.admin.record.HttpRecordRepository;
import org.apache.shenyu.disruptor.consumer.QueueConsumerExecutor;
import org.apache.shenyu.disruptor.consumer.QueueConsumerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

public class ShenyuHttpRequestRecordConsumerExecutor extends QueueConsumerExecutor<ShenyuHttpRequestRecordDTO> {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuHttpRequestRecordConsumerExecutor.class);

    private final HttpRecordProperties properties;

    private final Map<String, HttpRecordRepository> repositoryMap;

    public ShenyuHttpRequestRecordConsumerExecutor(final HttpRecordProperties properties, final Map<String, HttpRecordRepository> repositoryMap) {
        this.properties = properties;
        this.repositoryMap = repositoryMap;
    }

    @Override
    public void run() {
        ShenyuHttpRequestRecordDTO dto = getData();
        String storageType = StringUtils.isNotBlank(properties.getStorageType()) ? properties.getStorageType() : "local";
        HttpRecordRepository repository = repositoryMap.getOrDefault(storageType, repositoryMap.get("local"));
        repository.save(dto);
    }

    public static class ShenyuHttpRequestRecordConsumerExecutorFactory implements QueueConsumerFactory<ShenyuHttpRequestRecordDTO> {

        private final HttpRecordProperties properties;

        private final Map<String, HttpRecordRepository> repositoryMap;

        public ShenyuHttpRequestRecordConsumerExecutorFactory(final HttpRecordProperties properties, final Map<String, HttpRecordRepository> repositoryMap) {
            this.properties = properties;
            this.repositoryMap = repositoryMap;
        }

        @Override
        public QueueConsumerExecutor<ShenyuHttpRequestRecordDTO> create() {
            return new ShenyuHttpRequestRecordConsumerExecutor(properties, repositoryMap);
        }


        @Override
        public String fixName() {
            return "shenyu_http_request_record";
        }

    }
}
