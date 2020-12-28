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

package org.dromara.soul.admin.service.impl;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.SpringCloudRegisterDTO;
import org.dromara.soul.admin.dto.SpringMvcRegisterDTO;
import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.dto.SelectorDTO;
import org.dromara.soul.admin.dto.SelectorConditionDTO;
import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.admin.dto.RuleConditionDTO;
import org.dromara.soul.admin.entity.MetaDataDO;
import org.dromara.soul.admin.entity.PluginDO;
import org.dromara.soul.admin.entity.RuleDO;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.listener.DataChangedEvent;
import org.dromara.soul.admin.mapper.MetaDataMapper;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.admin.transfer.MetaDataTransfer;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.dto.convert.rule.RuleHandle;
import org.dromara.soul.common.dto.convert.rule.RuleHandleFactory;
import org.dromara.soul.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.enums.SelectorTypeEnum;
import org.dromara.soul.common.enums.MatchModeEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.ParamTypeEnum;
import org.dromara.soul.common.enums.OperatorEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.UUIDUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * The type Soul client register service.
 *
 * @author nuo-promise
 */
@Service("soulClientRegisterService")
public class SoulClientRegisterServiceImpl implements SoulClientRegisterService {

    private final MetaDataMapper metaDataMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final SelectorService selectorService;

    private final RuleService ruleService;

    private final RuleMapper ruleMapper;

    private final UpstreamCheckService upstreamCheckService;

    private final SelectorMapper selectorMapper;

    private final PluginMapper pluginMapper;




    /**
     * Instantiates a new Meta data service.
     *
     * @param metaDataMapper       the meta data mapper
     * @param eventPublisher       the event publisher
     * @param selectorService      the selector service
     * @param ruleService          the rule service
     * @param ruleMapper           the rule mapper
     * @param upstreamCheckService the upstream check service
     * @param selectorMapper       the selector mapper
     * @param pluginMapper         the plugin mapper
     */
    @Autowired(required = false)
    public SoulClientRegisterServiceImpl(final MetaDataMapper metaDataMapper,
                                         final ApplicationEventPublisher eventPublisher,
                                         final SelectorService selectorService,
                                         final RuleService ruleService,
                                         final RuleMapper ruleMapper,
                                         final UpstreamCheckService upstreamCheckService,
                                         final SelectorMapper selectorMapper,
                                         final PluginMapper pluginMapper) {
        this.metaDataMapper = metaDataMapper;
        this.eventPublisher = eventPublisher;
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.ruleMapper = ruleMapper;
        this.upstreamCheckService = upstreamCheckService;
        this.selectorMapper = selectorMapper;
        this.pluginMapper = pluginMapper;
    }

    @Override
    @Transactional
    public String registerSpringMvc(final SpringMvcRegisterDTO dto) {
        if (dto.isRegisterMetaData()) {
            MetaDataDO exist = metaDataMapper.findByPath(dto.getPath());
            if (Objects.isNull(exist)) {
                saveSpringMvcMetaData(dto);
            }
        }
        String selectorId = handlerSpringMvcSelector(dto);
        handlerSpringMvcRule(selectorId, dto);
        return SoulResultMessage.SUCCESS;
    }

    @Override
    @Transactional
    public synchronized String registerSpringCloud(final SpringCloudRegisterDTO dto) {
        MetaDataDO metaDataDO = metaDataMapper.findByPath(dto.getContext() + "/**");
        if (Objects.isNull(metaDataDO)) {
            saveSpringCloudMetaData(dto);
        }
        String selectorId = handlerSpringCloudSelector(dto);
        handlerSpringCloudRule(selectorId, dto);
        return SoulResultMessage.SUCCESS;
    }

    @Override
    @Transactional
    public String registerDubbo(final MetaDataDTO dto) {
        MetaDataDO exist = metaDataMapper.findByPath(dto.getPath());
        saveOrUpdateMetaData(exist, dto);
        String selectorId = handlerDubboSelector(dto);
        handlerDubboRule(selectorId, dto);
        return SoulResultMessage.SUCCESS;
    }

    private String handlerDubboSelector(final MetaDataDTO metaDataDTO) {
        return getString(metaDataDTO);
    }

    private String getString(final MetaDataDTO metaDataDTO) {
        SelectorDO selectorDO = selectorService.findByName(metaDataDTO.getContextPath());
        String selectorId;
        if (Objects.isNull(selectorDO)) {
            selectorId = registerSelector(metaDataDTO.getContextPath(), metaDataDTO.getRpcType(), metaDataDTO.getAppName(), "");
        } else {
            selectorId = selectorDO.getId();
        }
        return selectorId;
    }

    private void handlerDubboRule(final String selectorId, final MetaDataDTO metaDataDTO) {
        RuleDO existRule = ruleMapper.findByName(metaDataDTO.getPath());
        if (Objects.isNull(existRule)) {
            registerRule(selectorId, metaDataDTO.getPath(), metaDataDTO.getRpcType(), metaDataDTO.getRuleName());
        }
    }

    @Override
    public String registerSofa(final MetaDataDTO dto) {
        MetaDataDO byPath = metaDataMapper.findByPath(dto.getPath());
        if (Objects.nonNull(byPath)
                && (!byPath.getMethodName().equals(dto.getMethodName())
                || !byPath.getServiceName().equals(dto.getServiceName()))) {
            return "you path already exist!";
        }
        final MetaDataDO exist = metaDataMapper.findByServiceNameAndMethod(dto.getServiceName(), dto.getMethodName());
        saveOrUpdateMetaData(exist, dto);
        String selectorId = handlerSofaSelector(dto);
        handlerSofaRule(selectorId, dto, exist);
        return SoulResultMessage.SUCCESS;
    }

    @Override
    public String registerTars(final MetaDataDTO dto) {
        MetaDataDO byPath = metaDataMapper.findByPath(dto.getPath());
        if (Objects.nonNull(byPath)
                && (!byPath.getMethodName().equals(dto.getMethodName())
                || !byPath.getServiceName().equals(dto.getServiceName()))) {
            return "you path already exist!";
        }
        final MetaDataDO exist = metaDataMapper.findByServiceNameAndMethod(dto.getServiceName(), dto.getMethodName());
        saveOrUpdateMetaData(exist, dto);
        String selectorId = handlerTarsSelector(dto);
        handlerTarsRule(selectorId, dto, exist);
        return SoulResultMessage.SUCCESS;
    }

    private String handlerTarsSelector(final MetaDataDTO metaDataDTO) {
        return getString(metaDataDTO);
    }

    private void handlerTarsRule(final String selectorId, final MetaDataDTO metaDataDTO, final MetaDataDO exist) {
        RuleDO existRule = ruleMapper.findByName(metaDataDTO.getPath());
        if (Objects.isNull(exist) || Objects.isNull(existRule)) {
            registerRule(selectorId, metaDataDTO.getPath(), metaDataDTO.getRpcType(), metaDataDTO.getRuleName());
        }
    }

    private String handlerSofaSelector(final MetaDataDTO metaDataDTO) {
        return getString(metaDataDTO);
    }

    private void handlerSofaRule(final String selectorId, final MetaDataDTO metaDataDTO, final MetaDataDO exist) {
        RuleDO existRule = ruleMapper.findByName(metaDataDTO.getPath());
        if (Objects.isNull(exist) || Objects.isNull(existRule)) {
            registerRule(selectorId, metaDataDTO.getPath(), metaDataDTO.getRpcType(), metaDataDTO.getRuleName());
        }
    }

    private void saveSpringMvcMetaData(final SpringMvcRegisterDTO dto) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        MetaDataDO metaDataDO = MetaDataDO.builder()
                .appName(dto.getAppName())
                .path(dto.getPath())
                .pathDesc(dto.getPathDesc())
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

    private void saveSpringCloudMetaData(final SpringCloudRegisterDTO dto) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        MetaDataDO metaDataDO = MetaDataDO.builder()
                .appName(dto.getAppName())
                .path(dto.getContext() + "/**")
                .pathDesc(dto.getAppName() + "spring cloud meta data info")
                .serviceName(dto.getAppName())
                .methodName(dto.getContext())
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

    private void saveOrUpdateMetaData(final MetaDataDO exist, final MetaDataDTO metaDataDTO) {
        DataEventTypeEnum eventType;
        MetaDataDO metaDataDO = MetaDataTransfer.INSTANCE.mapToEntity(metaDataDTO);
        if (Objects.isNull(exist)) {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            metaDataDO.setId(UUIDUtils.getInstance().generateShortUuid());
            metaDataDO.setDateCreated(currentTime);
            metaDataDO.setDateUpdated(currentTime);
            metaDataMapper.insert(metaDataDO);
            eventType = DataEventTypeEnum.CREATE;
        } else {
            metaDataDO.setId(exist.getId());
            metaDataMapper.update(metaDataDO);
            eventType = DataEventTypeEnum.UPDATE;
        }
        // publish AppAuthData's event
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.META_DATA, eventType,
                Collections.singletonList(MetaDataTransfer.INSTANCE.mapToData(metaDataDTO))));
    }

    private String handlerSpringMvcSelector(final SpringMvcRegisterDTO dto) {
        String contextPath = dto.getContext();
        SelectorDO selectorDO = selectorService.findByName(contextPath);
        String selectorId;
        String uri = String.join(":", dto.getHost(), String.valueOf(dto.getPort()));
        if (Objects.isNull(selectorDO)) {
            selectorId = registerSelector(contextPath, dto.getRpcType(), dto.getAppName(), uri);
        } else {
            selectorId = selectorDO.getId();
            //update upstream
            String handle = selectorDO.getHandle();
            String handleAdd;
            DivideUpstream addDivideUpstream = buildDivideUpstream(uri);
            SelectorData selectorData = selectorService.buildByName(contextPath);
            if (StringUtils.isBlank(handle)) {
                handleAdd = GsonUtils.getInstance().toJson(Collections.singletonList(addDivideUpstream));
            } else {
                List<DivideUpstream> exist = GsonUtils.getInstance().fromList(handle, DivideUpstream.class);
                for (DivideUpstream upstream : exist) {
                    if (upstream.getUpstreamUrl().equals(addDivideUpstream.getUpstreamUrl())) {
                        return selectorId;
                    }
                }
                exist.add(addDivideUpstream);
                handleAdd = GsonUtils.getInstance().toJson(exist);
            }
            selectorDO.setHandle(handleAdd);
            selectorData.setHandle(handleAdd);
            // update db
            selectorMapper.updateSelective(selectorDO);
            // submit upstreamCheck
            upstreamCheckService.submit(contextPath, addDivideUpstream);
            // publish change event.
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE,
                    Collections.singletonList(selectorData)));
        }
        return selectorId;
    }

    private void handlerSpringMvcRule(final String selectorId, final SpringMvcRegisterDTO dto) {
        RuleDO ruleDO = ruleMapper.findByName(dto.getRuleName());
        if (Objects.isNull(ruleDO)) {
            registerRule(selectorId, dto.getPath(), dto.getRpcType(), dto.getRuleName());
        }
    }

    private String handlerSpringCloudSelector(final SpringCloudRegisterDTO dto) {
        String contextPath = dto.getContext();
        SelectorDO selectorDO = selectorService.findByName(contextPath);
        if (Objects.isNull(selectorDO)) {
            return registerSelector(contextPath, dto.getRpcType(), dto.getAppName(), "");
        } else {
            return selectorDO.getId();
        }
    }

    private void handlerSpringCloudRule(final String selectorId, final SpringCloudRegisterDTO dto) {
        RuleDO ruleDO = ruleMapper.findByName(dto.getRuleName());
        if (Objects.isNull(ruleDO)) {
            registerRule(selectorId, dto.getPath(), dto.getRpcType(), dto.getRuleName());
        }
    }

    private String registerSelector(final String contextPath, final String rpcType, final String appName, final String uri) {
        SelectorDTO selectorDTO = SelectorDTO.builder()
                .name(contextPath)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .matchMode(MatchModeEnum.AND.getCode())
                .enabled(Boolean.TRUE)
                .loged(Boolean.TRUE)
                .continued(Boolean.TRUE)
                .sort(1)
                .build();
        if (RpcTypeEnum.DUBBO.getName().equals(rpcType)) {
            selectorDTO.setPluginId(getPluginId(PluginEnum.DUBBO.getName()));
        } else if (RpcTypeEnum.SPRING_CLOUD.getName().equals(rpcType)) {
            selectorDTO.setPluginId(getPluginId(PluginEnum.SPRING_CLOUD.getName()));
            selectorDTO.setHandle(GsonUtils.getInstance().toJson(buildSpringCloudSelectorHandle(appName)));
        } else if (RpcTypeEnum.SOFA.getName().equals(rpcType)) {
            selectorDTO.setPluginId(getPluginId(PluginEnum.SOFA.getName()));
            selectorDTO.setHandle(appName);
        } else if (RpcTypeEnum.TARS.getName().equals(rpcType)) {
            selectorDTO.setPluginId(getPluginId(PluginEnum.TARS.getName()));
            selectorDTO.setHandle(appName);
        } else {
            //is divide
            DivideUpstream divideUpstream = buildDivideUpstream(uri);
            String handler = GsonUtils.getInstance().toJson(Collections.singletonList(divideUpstream));
            selectorDTO.setHandle(handler);
            selectorDTO.setPluginId(getPluginId(PluginEnum.DIVIDE.getName()));
            upstreamCheckService.submit(selectorDTO.getName(), divideUpstream);
        }
        SelectorConditionDTO selectorConditionDTO = new SelectorConditionDTO();
        selectorConditionDTO.setParamType(ParamTypeEnum.URI.getName());
        selectorConditionDTO.setParamName("/");
        selectorConditionDTO.setOperator(OperatorEnum.MATCH.getAlias());
        selectorConditionDTO.setParamValue(contextPath + "/**");
        selectorDTO.setSelectorConditions(Collections.singletonList(selectorConditionDTO));
        return selectorService.register(selectorDTO);
    }

    private DivideUpstream buildDivideUpstream(final String uri) {
        return DivideUpstream.builder()
                .upstreamHost("localhost")
                .protocol("http://")
                .upstreamUrl(uri)
                .weight(50)
                .build();
    }

    private SpringCloudSelectorHandle buildSpringCloudSelectorHandle(final String serviceId) {
        return SpringCloudSelectorHandle.builder()
                .serviceId(serviceId)
                .build();
    }

    private String getPluginId(final String pluginName) {
        final PluginDO pluginDO = pluginMapper.selectByName(pluginName);
        Objects.requireNonNull(pluginDO);
        return pluginDO.getId();
    }

    private void registerRule(final String selectorId, final String path, final String rpcType, final String ruleName) {
        RuleHandle ruleHandle = RuleHandleFactory.ruleHandle(RpcTypeEnum.acquireByName(rpcType), path);
        RuleDTO ruleDTO = RuleDTO.builder()
                .selectorId(selectorId)
                .name(ruleName)
                .matchMode(MatchModeEnum.AND.getCode())
                .enabled(Boolean.TRUE)
                .loged(Boolean.TRUE)
                .sort(1)
                .handle(ruleHandle.toJson())
                .build();
        RuleConditionDTO ruleConditionDTO = RuleConditionDTO.builder()
                .paramType(ParamTypeEnum.URI.getName())
                .paramName("/")
                .paramValue(path)
                .build();
        if (path.indexOf("*") > 1) {
            ruleConditionDTO.setOperator(OperatorEnum.MATCH.getAlias());
        } else {
            ruleConditionDTO.setOperator(OperatorEnum.EQ.getAlias());
        }
        ruleDTO.setRuleConditions(Collections.singletonList(ruleConditionDTO));
        ruleService.register(ruleDTO);
    }
}
