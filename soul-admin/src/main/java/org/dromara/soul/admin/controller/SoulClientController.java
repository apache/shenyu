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

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.disruptor.SoulServerMetaDataRegisterEventPublisher;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.register.common.dto.MetaDataRegisterDTO;
import org.dromara.soul.register.server.api.listener.DataChangedEvent;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * The type Soul client controller.
 *
 * @author xiaoyu
 */
@RestController
@RequestMapping("/soul-client")
@ConditionalOnProperty(prefix = "soul.register.registerType", value = "http", matchIfMissing = true)
public class SoulClientController {

    private static final SoulServerMetaDataRegisterEventPublisher PUBLISHER = SoulServerMetaDataRegisterEventPublisher.getInstance();
    
    /**
     * Instantiates a new Soul client controller.
     *
     * @param soulClientRegisterService the soul client register service
     */
    public SoulClientController(final SoulClientRegisterService soulClientRegisterService) {
        PUBLISHER.start(soulClientRegisterService);
    }
    
    /**
     * Register spring mvc string.
     *
     * @param metaDataRegisterDTO the meta data register dto
     * @return the string
     */
    @PostMapping("/springmvc-register")
    public String registerSpringMvc(@RequestBody final MetaDataRegisterDTO metaDataRegisterDTO) {
        publishEvent(RpcTypeEnum.HTTP, metaDataRegisterDTO);
        return SoulResultMessage.SUCCESS;
    }
    
    /**
     * Register spring cloud string.
     *
     * @param metaDataRegisterDTO the meta data register dto
     * @return the string
     */
    @PostMapping("/springcloud-register")
    public String registerSpringCloud(@RequestBody final MetaDataRegisterDTO metaDataRegisterDTO) {
        publishEvent(RpcTypeEnum.SPRING_CLOUD, metaDataRegisterDTO);
        return SoulResultMessage.SUCCESS;
    }
    
    /**
     * Register rpc string.
     *
     * @param metaDataRegisterDTO the meta data register dto
     * @return the string
     */
    @PostMapping("/dubbo-register")
    public String registerRpc(@RequestBody final MetaDataRegisterDTO metaDataRegisterDTO) {
        publishEvent(RpcTypeEnum.DUBBO, metaDataRegisterDTO);
        return SoulResultMessage.SUCCESS;
    }
    
    /**
     * Register sofa rpc string.
     *
     * @param metaDataRegisterDTO the meta data register dto
     * @return the string
     */
    @PostMapping("/sofa-register")
    public String registerSofaRpc(@RequestBody final MetaDataRegisterDTO metaDataRegisterDTO) {
        publishEvent(RpcTypeEnum.SOFA, metaDataRegisterDTO);
        return SoulResultMessage.SUCCESS;
    }
    
    /**
     * Register tars rpc string.
     *
     * @param metaDataRegisterDTO the meta data register dto
     * @return the string
     */
    @PostMapping("/tars-register")
    public String registerTarsRpc(@RequestBody final MetaDataRegisterDTO metaDataRegisterDTO) {
        publishEvent(RpcTypeEnum.TARS, metaDataRegisterDTO);
        return SoulResultMessage.SUCCESS;
    }
    
    /**
     * Register grpc string.
     *
     * @param metaDataRegisterDTO the meta data register dto
     * @return the string
     */
    @PostMapping("/grpc-register")
    public String registerGrpc(@RequestBody final MetaDataRegisterDTO metaDataRegisterDTO) {
        publishEvent(RpcTypeEnum.GRPC, metaDataRegisterDTO);
        return SoulResultMessage.SUCCESS;
    }
    
    private void publishEvent(final RpcTypeEnum rpcTypeEnum, final MetaDataRegisterDTO metaDataRegisterDTO) {
        PUBLISHER.publishEvent(DataChangedEvent.Type.REGISTER, rpcTypeEnum.getName(), metaDataRegisterDTO);
    }
}
