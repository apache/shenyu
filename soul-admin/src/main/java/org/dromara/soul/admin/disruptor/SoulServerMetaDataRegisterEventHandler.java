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
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.disruptor.AbstractDisruptorConsumerExecutor;
import org.dromara.soul.disruptor.DisruptorConsumerFactory;
import org.dromara.soul.register.server.api.listener.DataChangedEvent;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * SoulServerMetaDataRegisterEventHandler.
 *
 * @author tydhot
 * @author lw1243925457
 */
public class SoulServerMetaDataRegisterEventHandler extends AbstractDisruptorConsumerExecutor<DataChangedEvent> implements DisruptorConsumerFactory<DataChangedEvent> {

    private final SoulClientRegisterService soulClientRegisterService;

    private final SelectorService selectorService;

    private final SelectorMapper selectorMapper;

    private final ApplicationEventPublisher eventPublisher;

    public SoulServerMetaDataRegisterEventHandler(final SoulClientRegisterService soulClientRegisterService,
                                                  final SelectorService selectorService,
                                                  final SelectorMapper selectorMapper,
                                                  final ApplicationEventPublisher eventPublisher) {
        this.soulClientRegisterService = soulClientRegisterService;
        this.selectorService = selectorService;
        this.selectorMapper = selectorMapper;
        this.eventPublisher = eventPublisher;
    }

    @Override
    public void executor(final DataChangedEvent data) {
        if (data.getType().equals(DataChangedEvent.Type.REGISTER)) {
            register(data);
        } else if (data.getType().equals(DataChangedEvent.Type.UPDATED)) {
            update(data);
        }
    }

    private void register(final DataChangedEvent data) {
        if (data.getKey().equals(RpcTypeEnum.DUBBO.getName())) {
            soulClientRegisterService.registerDubbo((MetaDataDTO) data.getValue());
        } else if (data.getKey().equals(RpcTypeEnum.SOFA.getName())) {
            soulClientRegisterService.registerSofa((MetaDataDTO) data.getValue());
        } else if (data.getKey().equals(RpcTypeEnum.TARS.getName())) {
            soulClientRegisterService.registerTars((MetaDataDTO) data.getValue());
        } else if (data.getKey().equals(RpcTypeEnum.HTTP.getName())) {
            soulClientRegisterService.registerSpringMvc((SpringMvcRegisterDTO) data.getValue());
        } else if (data.getKey().equals(RpcTypeEnum.SPRING_CLOUD.getName())) {
            soulClientRegisterService.registerSpringCloud((SpringCloudRegisterDTO) data.getValue());
        } else if (data.getKey().equals(RpcTypeEnum.GRPC.getName())) {
            soulClientRegisterService.registerGrpc((MetaDataDTO) data.getValue());
        }
    }

    private void update(final DataChangedEvent data) {
        String contextPath = data.getKey();
        List<String> uriList = (List<String>) data.getValue();

        SelectorDO selector = selectorService.findByName(contextPath);
        if (Objects.nonNull(selector)) {
            SelectorData selectorData = selectorService.buildByName(contextPath);
            if (uriList == null) {
                selector.setHandle("");
                selectorData.setHandle("");
            } else {
                String handler = GsonUtils.getInstance().toJson(buildDivideUpstream(uriList));
                selector.setHandle(handler);
                selectorData.setHandle(handler);
            }
            selectorMapper.updateSelective(selector);
            // publish change event.
            eventPublisher.publishEvent(new org.dromara.soul.admin.listener.DataChangedEvent(ConfigGroupEnum.SELECTOR,
                    DataEventTypeEnum.UPDATE, Collections.singletonList(selectorData)));
        }
    }

    /**
     * build divide upstream list.
     *
     * @param uriList uri list
     * @return divide upstream list.
     */
    private List<DivideUpstream> buildDivideUpstream(final List<String> uriList) {
        return uriList.stream().map(uri -> DivideUpstream.builder()
                .upstreamHost("localhost")
                .protocol("http://")
                .upstreamUrl(uri)
                .weight(50)
                .build()).collect(Collectors.toList());
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
