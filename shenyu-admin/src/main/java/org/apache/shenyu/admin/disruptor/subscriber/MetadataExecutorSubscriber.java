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

package org.apache.shenyu.admin.disruptor.subscriber;

import org.apache.shenyu.admin.service.SoulClientRegisterService;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;

import java.util.Collection;

/**
 * The type Metadata executor subscriber.
 *
 * @author xiaoyu
 */
public class MetadataExecutorSubscriber implements ExecutorTypeSubscriber<MetaDataRegisterDTO> {
    
    private final SoulClientRegisterService soulClientRegisterService;
    
    public MetadataExecutorSubscriber(final SoulClientRegisterService soulClientRegisterService) {
        this.soulClientRegisterService = soulClientRegisterService;
    }
    
    @Override
    public DataType getType() {
        return DataType.META_DATA;
    }
    
    @Override
    public void executor(final Collection<MetaDataRegisterDTO> metaDataRegisterDTOList) {
        for (MetaDataRegisterDTO metaDataRegisterDTO : metaDataRegisterDTOList) {
            if (metaDataRegisterDTO.getRpcType().equals(RpcTypeEnum.DUBBO.getName())) {
                soulClientRegisterService.registerDubbo(metaDataRegisterDTO);
            } else if (metaDataRegisterDTO.getRpcType().equals(RpcTypeEnum.SOFA.getName())) {
                soulClientRegisterService.registerSofa(metaDataRegisterDTO);
            } else if (metaDataRegisterDTO.getRpcType().equals(RpcTypeEnum.TARS.getName())) {
                soulClientRegisterService.registerTars(metaDataRegisterDTO);
            } else if (metaDataRegisterDTO.getRpcType().equals(RpcTypeEnum.HTTP.getName())) {
                soulClientRegisterService.registerSpringMvc(metaDataRegisterDTO);
            } else if (metaDataRegisterDTO.getRpcType().equals(RpcTypeEnum.SPRING_CLOUD.getName())) {
                soulClientRegisterService.registerSpringCloud(metaDataRegisterDTO);
            } else if (metaDataRegisterDTO.getRpcType().equals(RpcTypeEnum.GRPC.getName())) {
                soulClientRegisterService.registerGrpc(metaDataRegisterDTO);
            } else if (metaDataRegisterDTO.getRpcType().equals(RpcTypeEnum.MOTAN.getName())) {
                soulClientRegisterService.registerMotan(metaDataRegisterDTO);
            }
        }
    }
}
