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
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ContextMappingRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.context.ApplicationEventPublisher;

import javax.annotation.Resource;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Abstract strategy.
 */
public abstract class AbstractShenyuClientRegisterServiceImpl implements ShenyuClientRegisterService {
    
    /**
     * The Event publisher.
     */
    @Resource
    public ApplicationEventPublisher eventPublisher;
    
    /**
     * The Meta data service.
     */
    @Resource
    public MetaDataService metaDataService;
    
    /**
     * The Selector service.
     */
    @Resource
    public SelectorService selectorService;
    
    /**
     * The Plugin service.
     */
    @Resource
    public PluginService pluginService;
    
    /**
     * The Rule service.
     */
    @Resource
    public RuleService ruleService;
    
    @Resource
    public UpstreamCheckService upstreamCheckService;
    
    /**
     * Plugin name string.
     *
     * @return the string
     */
    protected abstract String pluginName();
    
    /**
     * Selector handler string.
     *
     * @return the string
     */
    protected abstract String selectorHandler(MetaDataRegisterDTO metaDataDTO);
    
    /**
     * Rule handler string.
     *
     * @return the string
     */
    protected abstract String ruleHandler();
    
    /**
     * Register metadata.
     *
     * @param metaDataDTO the meta data dto
     */
    protected abstract void registerMetadata(MetaDataRegisterDTO metaDataDTO);
    
    /**
     * Build handle string.
     *
     * @param uriList the uri list
     * @param selectorDO the selector do
     * @return the string
     */
    protected abstract String buildHandle(List<URIRegisterDTO> uriList, SelectorDO selectorDO);
    
    /**
     * Register meta data.
     *
     * @param dto meta data register dto.
     * @return the string
     */
    @Override
    public String register(final MetaDataRegisterDTO dto) {
        //handler plugin selector
        String selectorHandler = selectorHandler(dto);
        String selectorId = selectorService.registerDefault(dto, pluginName(), selectorHandler);
        //handler selector rule
        String ruleHandler = ruleHandler();
        RuleDTO ruleDTO = buildDefaultRuleDTO(selectorId, dto, ruleHandler);
        ruleService.registerDefault(ruleDTO);
        //handler register metadata
        if (dto.isRegisterMetaData()) {
            registerMetadata(dto);
        }
        //handler context path
        String contextPath = dto.getContextPath();
        if (StringUtils.isNotEmpty(contextPath)) {
            String contextPathSelectorId = selectorService.registerDefault(dto, PluginEnum.CONTEXT_PATH.getName(), "");
            ContextMappingRuleHandle handle = new ContextMappingRuleHandle();
            handle.setContextPath(contextPath);
            ruleService.registerDefault(buildDefaultRuleDTO(contextPathSelectorId, dto, handle.toJson()));
        }
        return ShenyuResultMessage.SUCCESS;
    }
    
    /**
     * Register uri string.
     *
     * @param selectorName the selector name
     * @param uriList the uri list
     * @return the string
     */
    @Override
    public String registerURI(final String selectorName, final List<URIRegisterDTO> uriList) {
        if (CollectionUtils.isEmpty(uriList)) {
            return "";
        }
        SelectorDO selectorDO = selectorService.findByNameAndPluginName(selectorName, pluginName());
        if (Objects.isNull(selectorDO)) {
            return "";
        }
        // fetch UPSTREAM_MAP data from db
        //upstreamCheckService.fetchUpstreamData();
        //update upstream
        String handler = buildHandle(uriList, selectorDO);
        selectorDO.setHandle(handler);
        SelectorData selectorData = selectorService.buildByName(selectorName, pluginName());
        selectorData.setHandle(handler);
        // update db
        selectorService.updateSelective(selectorDO);
        // publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE, Collections.singletonList(selectorData)));
        return ShenyuResultMessage.SUCCESS;
    }
    
    protected void doSubmit(String selectorId, final List<? extends CommonUpstream > upstreamList) {
        List<CommonUpstream> commonUpstreamList = CommonUpstreamUtils.convertCommonUpstreamList(upstreamList);
        commonUpstreamList.forEach(upstream ->  upstreamCheckService.submit(selectorId, upstream));
    }
    
    private RuleDTO buildDefaultRuleDTO(final String selectorId, final MetaDataRegisterDTO metaDataDTO, final String ruleHandler) {
        String path = metaDataDTO.getPath();
        RuleDTO ruleDTO = RuleDTO.builder()
                .selectorId(selectorId)
                .name(metaDataDTO.getRuleName())
                .matchMode(MatchModeEnum.AND.getCode())
                .enabled(Boolean.TRUE)
                .loged(Boolean.TRUE)
                .sort(1)
                .handle(ruleHandler)
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
        return ruleDTO;
    }
}
