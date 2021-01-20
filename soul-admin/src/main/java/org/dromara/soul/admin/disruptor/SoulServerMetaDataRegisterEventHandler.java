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

import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.dto.SpringCloudRegisterDTO;
import org.dromara.soul.admin.dto.SpringMvcRegisterDTO;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.disruptor.AbstractDisruptorConsumerExecutor;
import org.dromara.soul.disruptor.DisruptorConsumerFactory;

/**
 * SoulServerMetaDataRegisterEventHandler.
 *
 * @author tydhot
 */
public class SoulServerMetaDataRegisterEventHandler extends AbstractDisruptorConsumerExecutor<SoulServerMetaDataRegisterEvent> implements DisruptorConsumerFactory<SoulServerMetaDataRegisterEvent> {
    private final SoulClientRegisterService soulClientRegisterService;

    public SoulServerMetaDataRegisterEventHandler(final SoulClientRegisterService soulClientRegisterService) {
        this.soulClientRegisterService = soulClientRegisterService;
    }

    @Override
    public void executor(final SoulServerMetaDataRegisterEvent data) {
        if (data.getType().equals(RpcTypeEnum.DUBBO.getName())) {
            soulClientRegisterService.registerDubbo((MetaDataDTO) data.getMetadata());
        } else if (data.getType().equals(RpcTypeEnum.SOFA.getName())) {
            soulClientRegisterService.registerSofa((MetaDataDTO) data.getMetadata());
        } else if (data.getType().equals(RpcTypeEnum.TARS.getName())) {
            soulClientRegisterService.registerTars((MetaDataDTO) data.getMetadata());
        } else if (data.getType().equals(RpcTypeEnum.HTTP.getName())) {
            soulClientRegisterService.registerSpringMvc((SpringMvcRegisterDTO) data.getMetadata());
        } else if (data.getType().equals(RpcTypeEnum.SPRING_CLOUD.getName())) {
            soulClientRegisterService.registerSpringCloud((SpringCloudRegisterDTO) data.getMetadata());
        }
    }

    @Override
    public String fixName() {
        return "soulServerMetaDataRegisterEventHandler";
    }

    @Override
    public AbstractDisruptorConsumerExecutor<SoulServerMetaDataRegisterEvent> create() {
        return this;
    }
}
