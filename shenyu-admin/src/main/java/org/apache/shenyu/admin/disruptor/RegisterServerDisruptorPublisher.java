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

import org.apache.shenyu.admin.disruptor.executor.RegisterServerConsumerExecutor.RegisterServerExecutorFactory;
import org.apache.shenyu.admin.disruptor.subscriber.MetadataExecutorSubscriber;
import org.apache.shenyu.admin.disruptor.subscriber.URIRegisterExecutorSubscriber;
import org.apache.shenyu.admin.service.SoulClientRegisterService;
import org.apache.shenyu.disruptor.DisruptorProviderManage;
import org.apache.shenyu.disruptor.provider.DisruptorProvider;
import org.apache.shenyu.register.server.api.SoulServerRegisterPublisher;

/**
 * The type Disruptor publisher.
 * 
 * @author xiaoyu
 */
@SuppressWarnings("all")
public class RegisterServerDisruptorPublisher implements SoulServerRegisterPublisher {

    private static final RegisterServerDisruptorPublisher INSTANCE = new RegisterServerDisruptorPublisher();

    private DisruptorProviderManage providerManage;
    
    private RegisterServerExecutorFactory factory;
    
    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static RegisterServerDisruptorPublisher getInstance() {
        return INSTANCE;
    }
    
    /**
     * start.
     *
     * @param soulClientRegisterService the soul client register service
     */
    public void start(final SoulClientRegisterService soulClientRegisterService) {
        factory = new RegisterServerExecutorFactory();
        factory.addSubscribers(new URIRegisterExecutorSubscriber(soulClientRegisterService));
        factory.addSubscribers(new MetadataExecutorSubscriber(soulClientRegisterService));
        providerManage = new DisruptorProviderManage(factory);
        providerManage.startup();
    }
    
    @Override
    public <T> void publish(final T data) {
        DisruptorProvider<Object> provider = providerManage.getProvider();
        provider.onData(f -> f.setData(data));
    }
    
    @Override
    public void close() {
        providerManage.getProvider().shutdown();
    }
}
