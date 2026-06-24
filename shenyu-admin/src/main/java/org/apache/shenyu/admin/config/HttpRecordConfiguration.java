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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.config.properties.HttpRecordProperties;
import org.apache.shenyu.admin.disruptor.ShenyuHttpRequestRecordDisruptorPublisher;
import org.apache.shenyu.admin.record.HttpRecordRepository;
import org.apache.shenyu.common.concurrent.MemorySafeTaskQueue;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.Singleton;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

@Configuration
public class HttpRecordConfiguration {

    @Bean
    public ShenyuHttpRequestRecordDisruptorPublisher shenyuHttpRequestRecordDisruptorPublisher(
            final HttpRecordProperties properties,
            final Map<String, HttpRecordRepository> repositoryMap) {
        ShenyuHttpRequestRecordDisruptorPublisher publisher = new ShenyuHttpRequestRecordDisruptorPublisher();
        publisher.start(properties, repositoryMap);
        return publisher;
    }

    @Bean(name = "replayExecutor")
    public ShenyuThreadPoolExecutor replayExecutor() {
        ShenyuConfig config = Optional.ofNullable(Singleton.INST.get(ShenyuConfig.class)).orElse(new ShenyuConfig());
        final ShenyuConfig.SharedPool sharedPool = config.getSharedPool();
        return new ShenyuThreadPoolExecutor(sharedPool.getCorePoolSize(),
                sharedPool.getMaximumPoolSize(), sharedPool.getKeepAliveTime(), TimeUnit.MILLISECONDS,
                new MemorySafeTaskQueue<>(Constants.THE_256_MB),
                ShenyuThreadFactory.create(config.getSharedPool().getPrefix(), true),
                new ThreadPoolExecutor.AbortPolicy());
    }

}
