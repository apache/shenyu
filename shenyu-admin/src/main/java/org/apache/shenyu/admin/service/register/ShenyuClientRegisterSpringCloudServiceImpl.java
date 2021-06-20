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

package org.apache.shenyu.admin.service.register;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.transfer.MetaDataTransfer;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.Objects;

/**
 * spring cloud service register.
 *
 * @author KevinClair
 **/
@Service("springCloud")
public class ShenyuClientRegisterSpringCloudServiceImpl extends AbstractShenyuClientRegisterService {

    private final MetaDataMapper metaDataMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final SelectorService selectorService;

    private final RuleService ruleService;

    private final RuleMapper ruleMapper;

    private final PluginMapper pluginMapper;

    public ShenyuClientRegisterSpringCloudServiceImpl(final MetaDataMapper metaDataMapper,
                                                      final ApplicationEventPublisher eventPublisher,
                                                      final SelectorService selectorService,
                                                      final RuleService ruleService,
                                                      final RuleMapper ruleMapper,
                                                      final PluginMapper pluginMapper) {
        this.metaDataMapper = metaDataMapper;
        this.eventPublisher = eventPublisher;
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.ruleMapper = ruleMapper;
        this.pluginMapper = pluginMapper;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public synchronized String register(final MetaDataRegisterDTO dto) {
        MetaDataDO metaDataDO = metaDataMapper.findByPath(dto.getContextPath() + "/**");
        if (Objects.isNull(metaDataDO)) {
            saveOrUpdateMetaData(metaDataDO, dto);
        }
        String selectorId = handlerSelector(dto);
        handlerRule(selectorId, dto, metaDataDO);
        String contextPath = dto.getContextPath();
        if (StringUtils.isNotEmpty(contextPath)) {
            //register context path plugin
            registerContextPathPlugin(contextPath);
        }
        return ShenyuResultMessage.SUCCESS;
    }

    @Override
    public void saveOrUpdateMetaData(final MetaDataDO exist, final MetaDataRegisterDTO dto) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        MetaDataDO metaDataDO = MetaDataDO.builder()
                .appName(dto.getAppName())
                .path(dto.getContextPath() + "/**")
                .pathDesc(dto.getAppName() + "spring cloud meta data info")
                .serviceName(dto.getAppName())
                .methodName(dto.getContextPath())
                .rpcType(dto.getRpcType())
                .enabled(dto.isEnabled())
                .id(UUIDUtils.getInstance().generateShortUuid())
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .build();
        metaDataMapper.insert(metaDataDO);
        // publish AppAuthData's event
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.META_DATA, DataEventTypeEnum.CREATE,
                Collections.singletonList(MetaDataTransfer.INSTANCE.mapToData(metaDataDO))));
    }

    @Override
    public String handlerSelector(final MetaDataRegisterDTO dto) {
        String contextPath = dto.getContextPath();
        if (StringUtils.isEmpty(contextPath)) {
            contextPath = buildContextPath(dto.getPath());
        }
        SelectorDO selectorDO = selectorService.findByName(contextPath);
        if (Objects.nonNull(selectorDO)) {
            return selectorDO.getId();
        }
        SelectorDTO selectorDTO = buildDefaultSelectorDTO(contextPath);
        selectorDTO.setPluginId(getPluginId(PluginEnum.SPRING_CLOUD.getName()));
        selectorDTO.setHandle(GsonUtils.getInstance().toJson(buildSpringCloudSelectorHandle(dto.getAppName())));
        selectorDTO.setSelectorConditions(buildDefaultSelectorConditionDTO(contextPath));
        return selectorService.register(selectorDTO);
    }

    @Override
    public String getPluginId(final String pluginName) {
        final PluginDO pluginDO = pluginMapper.selectByName(pluginName);
        Objects.requireNonNull(pluginDO);
        return pluginDO.getId();
    }

    @Override
    public void handlerRule(final String selectorId, final MetaDataRegisterDTO dto, final MetaDataDO exist) {
        RuleDO ruleDO = ruleMapper.findByName(dto.getRuleName());
        if (Objects.isNull(ruleDO)) {
            ruleService.register(registerRpcRule(selectorId, dto.getPath(), PluginEnum.SPRING_CLOUD.getName(), dto.getRuleName()));
        }
    }

    private void registerContextPathPlugin(final String contextPath) {
        String name = CONTEXT_PATH_NAME_PREFIX + contextPath;
        SelectorDO selectorDO = selectorService.findByName(name);
        if (Objects.isNull(selectorDO)) {
            String contextPathSelectorId = registerContextPathSelector(contextPath, name);
            RuleDO ruleDO = ruleMapper.findByName(name);
            if (Objects.isNull(ruleDO)) {
                ruleService.register(registerRpcRule(contextPathSelectorId, contextPath + "/**", PluginEnum.CONTEXT_PATH.getName(), name));
            }
        }
    }

    private String registerContextPathSelector(final String contextPath, final String name) {
        SelectorDTO selectorDTO = buildDefaultSelectorDTO(name);
        selectorDTO.setPluginId(getPluginId(PluginEnum.CONTEXT_PATH.getName()));
        selectorDTO.setSelectorConditions(buildDefaultSelectorConditionDTO(contextPath));
        return selectorService.register(selectorDTO);
    }

    private SpringCloudSelectorHandle buildSpringCloudSelectorHandle(final String serviceId) {
        return SpringCloudSelectorHandle.builder().serviceId(serviceId).build();
    }
}
