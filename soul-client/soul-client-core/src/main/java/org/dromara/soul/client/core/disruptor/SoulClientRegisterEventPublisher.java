/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.client.core.disruptor;

import org.dromara.soul.register.common.dto.MetaDataDTO;
import org.dromara.soul.disruptor.DisruptorProviderManage;
import org.dromara.soul.register.client.api.SoulClientRegisterRepository;

/**
 * The type Soul client register event publisher.
 *
 * @author tydhot
 */
public class SoulClientRegisterEventPublisher {
    
    private static final SoulClientRegisterEventPublisher INSTANCE = new SoulClientRegisterEventPublisher();

    private DisruptorProviderManage<SoulClientRegisterEvent> disruptorProviderManage;
    
    /**
     * get instance.
     *
     * @return SoulClientRegisterEventPublisher instance
     */
    public static SoulClientRegisterEventPublisher getInstance() {
        return INSTANCE;
    }
    
    /**
     * start.
     *
     * @param soulClientRegisterRepository soulClientRegisterRepository
     */
    public void start(final SoulClientRegisterRepository soulClientRegisterRepository) {
        disruptorProviderManage =
                new DisruptorProviderManage<>(
                        new SoulMetaDataRegisterEventHandler(soulClientRegisterRepository), 1, 4096 * 2 * 2);
        disruptorProviderManage.startup();
    }
    
    /**
     * publish event.
     *
     * @param metaDataDTO metaDataDTO
     */
    public void publishEvent(final MetaDataDTO metaDataDTO) {
        SoulClientRegisterEvent event = new SoulClientRegisterEvent();
        event.setMetaData(metaDataDTO);
        push(event);
    }

    private void push(final SoulClientRegisterEvent event) {
        disruptorProviderManage.getProvider().onData(event);
    }
}
