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

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.HttpRegisterDTO;
import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.dto.RuleConditionDTO;
import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.admin.dto.SelectorConditionDTO;
import org.dromara.soul.admin.dto.SelectorDTO;
import org.dromara.soul.admin.entity.MetaDataDO;
import org.dromara.soul.admin.entity.RuleDO;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.listener.DataChangedEvent;
import org.dromara.soul.admin.mapper.MetaDataMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.admin.transfer.MetaDataTransfer;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.dto.convert.rule.DivideRuleHandle;
import org.dromara.soul.common.dto.convert.rule.DubboRuleHandle;
import org.dromara.soul.common.dto.convert.rule.SpringCloudRuleHandle;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.enums.LoadBalanceEnum;
import org.dromara.soul.common.enums.MatchModeEnum;
import org.dromara.soul.common.enums.OperatorEnum;
import org.dromara.soul.common.enums.ParamTypeEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.enums.SelectorTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.JsonUtils;
import org.dromara.soul.common.utils.UUIDUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

/**
 * The type Soul client register service.
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
    
    /**
     * Instantiates a new Meta data service.
     *
     * @param metaDataMapper  the meta data mapper
     * @param eventPublisher  the event publisher
     * @param selectorService the selector service
     * @param ruleService     the rule service
     * @param ruleMapper      the rule mapper
     */
    @Autowired(required = false)
    public SoulClientRegisterServiceImpl(final MetaDataMapper metaDataMapper,
                                         final ApplicationEventPublisher eventPublisher,
                                         final SelectorService selectorService,
                                         final RuleService ruleService,
                                         final RuleMapper ruleMapper,
                                         final UpstreamCheckService upstreamCheckService,
                                         final SelectorMapper selectorMapper) {
        this.metaDataMapper = metaDataMapper;
        this.eventPublisher = eventPublisher;
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.ruleMapper = ruleMapper;
        this.upstreamCheckService = upstreamCheckService;
        this.selectorMapper = selectorMapper;
    }
    
    @Override
    public String registerHttp(final HttpRegisterDTO httpRegisterDTO) {
        String selectorId = handlerHttpSelector(httpRegisterDTO);
        handlerHttpRule(selectorId, httpRegisterDTO);
        return "success";
    }
    
    @Override
    public String registerRpc(final MetaDataDTO metaDataDTO) {
        MetaDataDO byPath = metaDataMapper.findByPath(metaDataDTO.getPath());
        if (Objects.nonNull(byPath)
                && (!byPath.getMethodName().equals(metaDataDTO.getMethodName())
                || !byPath.getServiceName().equals(metaDataDTO.getServiceName()))) {
            return "you path already exist!";
        }
        final MetaDataDO exist = metaDataMapper.findByServiceNameAndMethod(metaDataDTO.getServiceName(), metaDataDTO.getMethodName());
        saveOrUpdateMetaData(exist, metaDataDTO);
        String selectorId = handlerDubboSelector(metaDataDTO);
        handlerDubboRule(selectorId, metaDataDTO, exist);
        return "success";
    }
    
    private String handlerDubboSelector(final MetaDataDTO metaDataDTO) {
        SelectorDO selectorDO = selectorService.findByName(metaDataDTO.getContextPath());
        String selectorId;
        if (Objects.isNull(selectorDO)) {
            //需要新增
            selectorId = registerSelector(metaDataDTO.getContextPath(), metaDataDTO.getRpcType(), metaDataDTO.getAppName(), "");
        } else {
            selectorId = selectorDO.getId();
        }
        return selectorId;
    }
    
    private void handlerDubboRule(final String selectorId, final MetaDataDTO metaDataDTO, final MetaDataDO exist) {
        RuleDO existRule = ruleMapper.findByName(metaDataDTO.getPath());
        if (Objects.isNull(exist) || Objects.isNull(existRule)) {
            //需要新增
            registerRule(selectorId, metaDataDTO.getPath(), metaDataDTO.getRpcType(), metaDataDTO.getRuleName());
        }
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
    
    
    private String handlerHttpSelector(final HttpRegisterDTO httpRegisterDTO) {
        String contextPath = httpRegisterDTO.getContext();
        SelectorDO selectorDO = selectorService.findByName(contextPath);
        String selectorId;
        String uri = String.join(":", httpRegisterDTO.getHost(), String.valueOf(httpRegisterDTO.getPort()));
        if (Objects.isNull(selectorDO)) {
            //需要新增
            selectorId = registerSelector(contextPath, httpRegisterDTO.getRpcType(), httpRegisterDTO.getAppName(), uri);
        } else {
            selectorId = selectorDO.getId();
            if (RpcTypeEnum.HTTP.getName().equals(httpRegisterDTO.getRpcType())) {
                //更新 upstream
                String handle = selectorDO.getHandle();
                String handleAdd;
                DivideUpstream addDivideUpstream = buildDivideUpstream(uri);
                SelectorData selectorData = selectorService.buildByName(contextPath);
                if (StringUtils.isBlank(handle)) {
                    handleAdd = GsonUtils.getInstance().toJson(addDivideUpstream);
                } else {
                    List<DivideUpstream> divideUpstreams = GsonUtils.getInstance().fromList(handle, DivideUpstream.class);
                    divideUpstreams.add(addDivideUpstream);
                    handleAdd = GsonUtils.getInstance().toJson(divideUpstreams);
                }
                selectorDO.setHandle(handleAdd);
                selectorData.setHandle(handleAdd);
                //更新数据库
                selectorMapper.updateSelective(selectorDO);
                //提交过去检查
                upstreamCheckService.submit(contextPath, addDivideUpstream);
                //发送更新事件
                // publish change event.
                eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE,
                        Collections.singletonList(selectorData)));
            }
        }
        return selectorId;
    }
    
    private void handlerHttpRule(final String selectorId, final HttpRegisterDTO httpRegisterDTO) {
        RuleDO ruleDO = ruleMapper.findByName(httpRegisterDTO.getRuleName());
        if (Objects.isNull(ruleDO)) {
            //需要新增
            registerRule(selectorId, httpRegisterDTO.getPath(), httpRegisterDTO.getRpcType(), httpRegisterDTO.getRuleName());
        }
    }
    
    private String registerSelector(final String contextPath, final String rpcType, final String appName, final String uri) {
        SelectorDTO selectorDTO = new SelectorDTO();
        selectorDTO.setName(contextPath);
        selectorDTO.setType(SelectorTypeEnum.CUSTOM_FLOW.getCode());
        selectorDTO.setMatchMode(MatchModeEnum.AND.getCode());
        selectorDTO.setEnabled(Boolean.TRUE);
        selectorDTO.setLoged(Boolean.TRUE);
        selectorDTO.setContinued(Boolean.TRUE);
        selectorDTO.setSort(1);
        if (RpcTypeEnum.DUBBO.getName().equals(rpcType)) {
            selectorDTO.setPluginId("6");
        } else if (RpcTypeEnum.SPRING_CLOUD.getName().equals(rpcType)) {
            selectorDTO.setPluginId("8");
            selectorDTO.setHandle(appName);
        } else {
            //is divide
            DivideUpstream divideUpstream = buildDivideUpstream(uri);
            String handler = GsonUtils.getInstance().toJson(divideUpstream);
            selectorDTO.setHandle(handler);
            selectorDTO.setPluginId("5");
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
        DivideUpstream divideUpstream = new DivideUpstream();
        divideUpstream.setUpstreamHost("localhost");
        divideUpstream.setProtocol("http://");
        divideUpstream.setUpstreamUrl(uri);
        divideUpstream.setWeight(50);
        return divideUpstream;
    }
    
    private void registerRule(final String selectorId, final String path, final String rpcType, final String ruleName) {
        RuleDTO ruleDTO = new RuleDTO();
        ruleDTO.setSelectorId(selectorId);
        ruleDTO.setName(ruleName);
        ruleDTO.setMatchMode(MatchModeEnum.AND.getCode());
        ruleDTO.setEnabled(Boolean.TRUE);
        ruleDTO.setLoged(Boolean.TRUE);
        ruleDTO.setSort(1);
        RuleConditionDTO ruleConditionDTO = new RuleConditionDTO();
        ruleConditionDTO.setParamType(ParamTypeEnum.URI.getName());
        ruleConditionDTO.setParamName("/");
        if (path.indexOf("*") > 1) {
            ruleConditionDTO.setOperator(OperatorEnum.MATCH.getAlias());
        } else {
            ruleConditionDTO.setOperator(OperatorEnum.EQ.getAlias());
        }
        ruleConditionDTO.setParamValue(path);
        ruleDTO.setRuleConditions(Collections.singletonList(ruleConditionDTO));
        if (rpcType.equals(RpcTypeEnum.DUBBO.getName())) {
            DubboRuleHandle dubboRuleHandle = new DubboRuleHandle();
            dubboRuleHandle.setLoadBalance(LoadBalanceEnum.RANDOM.getName());
            dubboRuleHandle.setRetries(0);
            dubboRuleHandle.setTimeout(3000);
            ruleDTO.setHandle(JsonUtils.toJson(dubboRuleHandle));
        } else if (rpcType.equals(RpcTypeEnum.HTTP.getName())) {
            DivideRuleHandle divideRuleHandle = new DivideRuleHandle();
            divideRuleHandle.setLoadBalance(LoadBalanceEnum.RANDOM.getName());
            divideRuleHandle.setRetry(0);
            ruleDTO.setHandle(JsonUtils.toJson(divideRuleHandle));
        } else {
            SpringCloudRuleHandle springCloudRuleHandle = new SpringCloudRuleHandle();
            springCloudRuleHandle.setPath(path);
            ruleDTO.setHandle(JsonUtils.toJson(springCloudRuleHandle));
        }
        ruleService.register(ruleDTO);
    }
}
