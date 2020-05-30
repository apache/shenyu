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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.admin.dto.HttpRegisterDTO;
import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.dto.RuleConditionDTO;
import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.admin.dto.SelectorConditionDTO;
import org.dromara.soul.admin.dto.SelectorDTO;
import org.dromara.soul.admin.entity.RuleConditionDO;
import org.dromara.soul.admin.entity.RuleDO;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.listener.DataChangedEvent;
import org.dromara.soul.admin.mapper.MetaDataMapper;
import org.dromara.soul.admin.mapper.RuleConditionMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.query.RuleConditionQuery;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.admin.transfer.ConditionTransfer;
import org.dromara.soul.common.dto.ConditionData;
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
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.enums.SelectorTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.JsonUtils;
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
    
    private final RuleConditionMapper ruleConditionMapper;
    
    /**
     * Instantiates a new Meta data service.
     *
     * @param metaDataMapper      the meta data mapper
     * @param eventPublisher      the event publisher
     * @param selectorService     the selector service
     * @param ruleService         the rule service
     * @param ruleMapper          the rule mapper
     * @param ruleConditionMapper the rule condition mapper
     */
    @Autowired(required = false)
    public SoulClientRegisterServiceImpl(final MetaDataMapper metaDataMapper,
                                         final ApplicationEventPublisher eventPublisher,
                                         final SelectorService selectorService,
                                         final RuleService ruleService,
                                         final RuleMapper ruleMapper,
                                         final RuleConditionMapper ruleConditionMapper) {
        this.metaDataMapper = metaDataMapper;
        this.eventPublisher = eventPublisher;
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.ruleMapper = ruleMapper;
        this.ruleConditionMapper = ruleConditionMapper;
    }
    
    @Override
    public String registerHttp(HttpRegisterDTO httpRegisterDTO) {
        String selectorId = handlerHttpSelector(httpRegisterDTO);
        handlerHttpRule(selectorId, httpRegisterDTO);
        return "success";
    }
    
    @Override
    public String registerRpc(MetaDataDTO metaDataDTO) {
        return null;
    }
    
    private String handlerHttpSelector(final HttpRegisterDTO httpRegisterDTO) {
        String contextPath = httpRegisterDTO.getContext();
        SelectorDO selectorDO = selectorService.findByName(contextPath);
        String selectorId;
        if (Objects.isNull(selectorDO)) {
            //需要新增
            String uri = String.join(":", httpRegisterDTO.getHost(), String.valueOf(httpRegisterDTO.getPort()));
            selectorId = registerSelector(contextPath, httpRegisterDTO.getRpcType(), httpRegisterDTO.getAppName(), uri);
        } else {
            selectorId = selectorDO.getId();
        }
        return selectorId;
    }
    
    private void handlerHttpRule(final String selectorId, final HttpRegisterDTO httpRegisterDTO) {
        RuleDO ruleDO = ruleMapper.findByName(httpRegisterDTO.getRuleName());
        if (Objects.isNull(ruleDO)) {
            //需要新增
            registerRule(selectorId, httpRegisterDTO.getPath(), httpRegisterDTO.getRpcType(), httpRegisterDTO.getRuleName());
        } else {
            List<RuleConditionDO> ruleConditionDOS = ruleConditionMapper.selectByQuery(new RuleConditionQuery(ruleDO.getId()));
            if (CollectionUtils.isEmpty(ruleConditionDOS)) {
                return;
            }
            List<ConditionData> conditionDataList = new ArrayList<>();
            for (RuleConditionDO ruleConditionDO : ruleConditionDOS) {
                if (ruleConditionDO.getParamType().equals(ParamTypeEnum.URI.getName())
                        || !ruleConditionDO.getParamValue().equals(httpRegisterDTO.getPath())) {
                    ruleConditionDO.setParamValue(httpRegisterDTO.getPath());
                    ruleConditionMapper.updateSelective(ruleConditionDO);
                }
                conditionDataList.add(ConditionTransfer.INSTANCE.mapToRuleDO(ruleConditionDO));
            }
            if (CollectionUtils.isNotEmpty(conditionDataList)) {
                publishEvent(ruleDO, conditionDataList, httpRegisterDTO.getRpcType());
            }
        }
    }
    
    private void publishEvent(final RuleDO ruleDO, final List<ConditionData> conditionDataList, final String rpcType) {
        String pluginName;
        if (RpcTypeEnum.DUBBO.getName().equals(rpcType)) {
            pluginName = PluginEnum.DUBBO.getName();
        } else if (RpcTypeEnum.HTTP.getName().equals(rpcType)) {
            pluginName = PluginEnum.DIVIDE.getName();
        } else {
            pluginName = PluginEnum.SPRING_CLOUD.getName();
        }
        // publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.UPDATE,
                Collections.singletonList(RuleDO.transFrom(ruleDO, pluginName, conditionDataList))));
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
            DivideUpstream divideUpstream = new DivideUpstream();
            divideUpstream.setUpstreamHost("localhost");
            divideUpstream.setProtocol("http://");
            divideUpstream.setUpstreamUrl(uri);
            divideUpstream.setWeight(50);
            String handler = GsonUtils.getInstance().toJson(divideUpstream);
            selectorDTO.setHandle(handler);
            selectorDTO.setPluginId("5");
        }
        SelectorConditionDTO selectorConditionDTO = new SelectorConditionDTO();
        selectorConditionDTO.setParamType(ParamTypeEnum.URI.getName());
        selectorConditionDTO.setParamName("/");
        selectorConditionDTO.setOperator(OperatorEnum.MATCH.getAlias());
        selectorConditionDTO.setParamValue(contextPath + "/**");
        selectorDTO.setSelectorConditions(Collections.singletonList(selectorConditionDTO));
        return selectorService.register(selectorDTO);
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
