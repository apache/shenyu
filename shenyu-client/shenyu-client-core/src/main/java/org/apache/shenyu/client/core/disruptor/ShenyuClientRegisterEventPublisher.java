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

package org.apache.shenyu.client.core.disruptor;

import org.apache.shenyu.client.core.disruptor.executor.RegisterClientConsumerExecutor.RegisterClientExecutorFactory;
import org.apache.shenyu.client.core.disruptor.subcriber.ShenyuClientMetadataExecutorSubscriber;
import org.apache.shenyu.client.core.disruptor.subcriber.ShenyuClientURIExecutorSubscriber;
import org.apache.shenyu.disruptor.DisruptorProviderManage;
import org.apache.shenyu.disruptor.provider.DisruptorProvider;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.type.DataTypeParent;

/**
 * The type shenyu client register event publisher.
 */
public class ShenyuClientRegisterEventPublisher {
    
    private static final ShenyuClientRegisterEventPublisher INSTANCE = new ShenyuClientRegisterEventPublisher();
    
    private DisruptorProviderManage<DataTypeParent> providerManage;
    
    /**
     * Get instance.
     *
     * @return ShenyuClientRegisterEventPublisher instance
     */
    public static ShenyuClientRegisterEventPublisher getInstance() {
        return INSTANCE;
    }
    
    /**
     * Start.
     *
     * @param shenyuClientRegisterRepository shenyuClientRegisterRepository
     */
    public void start(final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        RegisterClientExecutorFactory factory = new RegisterClientExecutorFactory();
        factory.addSubscribers(new ShenyuClientMetadataExecutorSubscriber(shenyuClientRegisterRepository));
        factory.addSubscribers(new ShenyuClientURIExecutorSubscriber(shenyuClientRegisterRepository));
        providerManage = new DisruptorProviderManage<>(factory);
        providerManage.startup();
    }
    
    /**
     * Publish event.
     *
     * @param data the data
     */
    public void publishEvent(final DataTypeParent data) {
        DisruptorProvider<DataTypeParent> provider = providerManage.getProvider();
        provider.onData(data);
    }
}
