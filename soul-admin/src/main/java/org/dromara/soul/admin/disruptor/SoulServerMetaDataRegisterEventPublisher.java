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

package org.dromara.soul.admin.disruptor;

import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.disruptor.DisruptorProviderManage;
import org.dromara.soul.register.common.dto.MetaDataRegisterDTO;
import org.dromara.soul.register.server.api.SoulSeverRegisterCenterEventPublisher;
import org.dromara.soul.register.server.api.listener.DataChangedEvent;

/**
 * SoulServerMetaDataRegisterEventPublisher.
 *
 * @author tydhot
 * @author lw1243925457
 */
public class SoulServerMetaDataRegisterEventPublisher implements SoulSeverRegisterCenterEventPublisher {

    private static final SoulServerMetaDataRegisterEventPublisher INSTANCE = new SoulServerMetaDataRegisterEventPublisher();

    private DisruptorProviderManage<DataChangedEvent> disruptorProviderManage;

    /**
     * get instance.
     *
     * @return SoulServerRegisterEventPublisher
     */
    public static SoulServerMetaDataRegisterEventPublisher getInstance() {
        return INSTANCE;
    }

    /**
     * start.
     *
     * @param soulClientRegisterService the soul client register service
     */
    public void start(final SoulClientRegisterService soulClientRegisterService) {
        disruptorProviderManage =
                new DisruptorProviderManage<>(new SoulServerMetaDataRegisterEventHandler(soulClientRegisterService), 1, 4096 * 2 * 2);
        disruptorProviderManage.startup();
    }
    
    @Override
    public void publishEvent(final DataChangedEvent.Type eventType, final String key, final MetaDataRegisterDTO metaDataRegisterDTO) {
        push(new DataChangedEvent(key, metaDataRegisterDTO, eventType));
    }

    private void push(final DataChangedEvent event) {
        disruptorProviderManage.getProvider().onData(event);
    }
}
