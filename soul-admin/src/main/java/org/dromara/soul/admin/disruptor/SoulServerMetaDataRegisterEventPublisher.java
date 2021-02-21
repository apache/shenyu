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
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.disruptor.DisruptorProviderManage;
import org.dromara.soul.register.server.api.SoulSeverRegisterCenterEventPublisher;
import org.dromara.soul.register.server.api.listener.DataChangedEvent;
import org.springframework.context.ApplicationEventPublisher;

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
     * @param soulClientRegisterService soulClientRegisterService
     */
    public void start(final SoulClientRegisterService soulClientRegisterService,
                      final SelectorService selectorService,
                      final SelectorMapper selectorMapper,
                      final ApplicationEventPublisher eventPublisher) {
        disruptorProviderManage = new DisruptorProviderManage<>(
                new SoulServerMetaDataRegisterEventHandler(soulClientRegisterService, selectorService, selectorMapper, eventPublisher),
                1, 4096 * 2 * 2);
        disruptorProviderManage.startup();
    }

    /**
     * 注册中心应该只放入数据，数据应该在Admin中，那事件应该有多种类型：注册事件、更新事件（服务下线）
     * 所以这里增加了一个更新事件
     *
     * publish event.
     *
     * @param eventType data event type
     * @param key if register event, key is eventType; if update event, key is context path
     * @param value if register event, value is metadata; if update event, key is uri list
     */
    @Override
    public void publishEvent(final DataChangedEvent.Type eventType, final String key, final Object value) {
        // TODO:两种事件混用，参数和处理之类，给人感觉不够清晰明了
        DataChangedEvent event;
        if (eventType.equals(DataChangedEvent.Type.REGISTER)) {
            // 为了兼容HTTP注册方式：目前HTTP能直接得到Admin中的数据类型
            // 但ZK Register模块不应该依赖于Admin相关模块；因为ZK数据以字符串形式传入，所以在这里进行数据转换
            if (value instanceof String) {
                event = new DataChangedEvent(key, convertToDTO(key, value.toString()), eventType);
            } else {
                event = new DataChangedEvent(key, value, eventType);
            }
        } else {
            event = new DataChangedEvent(key, value, eventType);
        }
        push(event);
    }

    private Object convertToDTO(String type, String data) {
        if (type.equals(RpcTypeEnum.HTTP.getName())) {
            return GsonUtils.getInstance().fromJson(data, SpringMvcRegisterDTO.class);
        }
        if (type.equals(RpcTypeEnum.SPRING_CLOUD.getName())) {
            return GsonUtils.getInstance().fromJson(data, SpringCloudRegisterDTO.class);
        }
        return GsonUtils.getInstance().fromJson(data, MetaDataDTO.class);
    }

    private void push(final DataChangedEvent event) {
        disruptorProviderManage.getProvider().onData(event);
    }
}
