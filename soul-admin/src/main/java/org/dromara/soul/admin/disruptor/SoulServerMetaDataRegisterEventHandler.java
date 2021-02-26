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
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.disruptor.AbstractDisruptorConsumerExecutor;
import org.dromara.soul.disruptor.DisruptorConsumerFactory;
import org.dromara.soul.register.common.dto.MetaDataRegisterDTO;
import org.dromara.soul.register.server.api.listener.DataChangedEvent;

/**
 * SoulServerMetaDataRegisterEventHandler.
 *
 * @author tydhot
 * @author lw1243925457
 */
public class SoulServerMetaDataRegisterEventHandler extends AbstractDisruptorConsumerExecutor<DataChangedEvent> implements DisruptorConsumerFactory<DataChangedEvent> {

    private final SoulClientRegisterService soulClientRegisterService;
    
    /**
     * Instantiates a new Soul server meta data register event handler.
     *
     * @param soulClientRegisterService the soul client register service
     */
    public SoulServerMetaDataRegisterEventHandler(final SoulClientRegisterService soulClientRegisterService) {
        this.soulClientRegisterService = soulClientRegisterService;
    }

    @Override
    public void executor(final DataChangedEvent data) {
        register(data);
    }

    private void register(final DataChangedEvent data) {
        if (data.getKey().equals(RpcTypeEnum.DUBBO.getName())) {
            soulClientRegisterService.registerDubbo((MetaDataRegisterDTO) data.getValue());
        } else if (data.getKey().equals(RpcTypeEnum.SOFA.getName())) {
            soulClientRegisterService.registerSofa((MetaDataRegisterDTO) data.getValue());
        } else if (data.getKey().equals(RpcTypeEnum.TARS.getName())) {
            soulClientRegisterService.registerTars((MetaDataRegisterDTO) data.getValue());
        } else if (data.getKey().equals(RpcTypeEnum.HTTP.getName())) {
            soulClientRegisterService.registerSpringMvc((MetaDataRegisterDTO) data.getValue());
        } else if (data.getKey().equals(RpcTypeEnum.SPRING_CLOUD.getName())) {
            soulClientRegisterService.registerSpringCloud((MetaDataRegisterDTO) data.getValue());
        } else if (data.getKey().equals(RpcTypeEnum.GRPC.getName())) {
            soulClientRegisterService.registerGrpc((MetaDataRegisterDTO) data.getValue());
        }
    }
    
    @Override
    public String fixName() {
        return "soulServerMetaDataRegisterEventHandler";
    }

    @Override
    public AbstractDisruptorConsumerExecutor<DataChangedEvent> create() {
        return this;
    }
}
