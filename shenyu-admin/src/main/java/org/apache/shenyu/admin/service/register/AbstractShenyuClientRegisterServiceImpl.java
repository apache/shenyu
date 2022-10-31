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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.PathUtils;
import org.apache.shenyu.common.utils.PluginNameAdapter;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.context.ApplicationEventPublisher;

import javax.annotation.Resource;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Abstract strategy.
 */
public abstract class AbstractShenyuClientRegisterServiceImpl extends FallbackShenyuClientRegisterService implements ShenyuClientRegisterService {
    
    /**
     * The Event publisher.
     */
    @Resource
    private ApplicationEventPublisher eventPublisher;
    
    /**
     * The Selector service.
     */
    @Resource
    private SelectorService selectorService;
    
    @Resource
    private MetaDataService metaDataService;
    
    /**
     * The Rule service.
     */
    @Resource
    private RuleService ruleService;
    
    @Resource
    private UpstreamCheckService upstreamCheckService;
    
    /**
     * Selector handler string.
     *
     * @param metaDataDTO the meta data dto
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
     * @param uriList    the uri list
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
        String selectorId = selectorService.registerDefault(dto, PluginNameAdapter.rpcTypeAdapter(rpcType()), selectorHandler);
        //handler selector rule
        String ruleHandler = ruleHandler();
        RuleDTO ruleDTO = buildRpcDefaultRuleDTO(selectorId, dto, ruleHandler);
        ruleService.registerDefault(ruleDTO);
        //handler register metadata
        registerMetadata(dto);
        //handler context path
        String contextPath = dto.getContextPath();
        if (StringUtils.isNotEmpty(contextPath)) {
            registerContextPath(dto);
        }
        return ShenyuResultMessage.SUCCESS;
    }
    
    /**
     * Register uri string.
     *
     * @param selectorName the selector name
     * @param uriList      the uri list
     * @return the string
     */
    @Override
    public String doRegisterURI(final String selectorName, final List<URIRegisterDTO> uriList) {
        if (CollectionUtils.isEmpty(uriList)) {
            return "";
        }
        SelectorDO selectorDO = selectorService.findByNameAndPluginName(selectorName, PluginNameAdapter.rpcTypeAdapter(rpcType()));
        if (Objects.isNull(selectorDO)) {
            throw new ShenyuException("doRegister Failed to execute,wait to retry.");
        }
        // fetch UPSTREAM_MAP data from db
        //upstreamCheckService.fetchUpstreamData();
        //update upstream
        List<URIRegisterDTO> validUriList = uriList.stream().filter(dto -> Objects.nonNull(dto.getPort()) && StringUtils.isNotBlank(dto.getHost())).collect(Collectors.toList());
        String handler = buildHandle(validUriList, selectorDO);
        if (handler != null) {
            selectorDO.setHandle(handler);
            SelectorData selectorData = selectorService.buildByName(selectorName, PluginNameAdapter.rpcTypeAdapter(rpcType()));
            selectorData.setHandle(handler);
            // update db
            selectorService.updateSelective(selectorDO);
            // publish change event.
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE, Collections.singletonList(selectorData)));
        }
        return ShenyuResultMessage.SUCCESS;
    }
    
    /**
     * Gets meta data service.
     *
     * @return the meta data service
     */
    public MetaDataService getMetaDataService() {
        return metaDataService;
    }
    
    /**
     * Gets selector service.
     *
     * @return the selector service
     */
    public SelectorService getSelectorService() {
        return selectorService;
    }
    
    /**
     * Gets rule service.
     *
     * @return the rule service
     */
    public RuleService getRuleService() {
        return ruleService;
    }
    
    /**
     * Do submit.
     *
     * @param selectorId   the selector id
     * @param upstreamList the upstream list
     * @return whether this module handles
     */
    protected boolean doSubmit(final String selectorId, final List<? extends CommonUpstream> upstreamList) {
        List<CommonUpstream> commonUpstreamList = CommonUpstreamUtils.convertCommonUpstreamList(upstreamList);
        return commonUpstreamList.stream().map(upstream -> upstreamCheckService.checkAndSubmit(selectorId, upstream))
                .collect(Collectors.toList()).stream().findAny().orElse(false);
    }
    
    /**
     * Build context path default rule dto rule dto.
     *
     * @param selectorId  the selector id
     * @param metaDataDTO the meta data dto
     * @param ruleHandler the rule handler
     * @return the rule dto
     */
    protected RuleDTO buildContextPathDefaultRuleDTO(final String selectorId, final MetaDataRegisterDTO metaDataDTO, final String ruleHandler) {
        String contextPath = metaDataDTO.getContextPath();
        return buildRuleDTO(selectorId, ruleHandler, contextPath, PathUtils.decoratorPath(contextPath));
    }
    
    private RuleDTO buildRpcDefaultRuleDTO(final String selectorId, final MetaDataRegisterDTO metaDataDTO, final String ruleHandler) {
        return buildRuleDTO(selectorId, ruleHandler, metaDataDTO.getRuleName(), metaDataDTO.getPath());
    }
    
    private RuleDTO buildRuleDTO(final String selectorId, final String ruleHandler, final String ruleName, final String path) {
        RuleDTO ruleDTO = RuleDTO.builder()
                .selectorId(selectorId)
                .name(ruleName)
                .matchMode(MatchModeEnum.AND.getCode())
                .enabled(Boolean.TRUE)
                .loged(Boolean.TRUE)
                .sort(1)
                .handle(ruleHandler)
                .build();
        RuleConditionDTO ruleConditionDTO = RuleConditionDTO.builder()
                .paramType(ParamTypeEnum.URI.getName())
                .paramName("/")
                .paramValue(this.rewritePath(path))
                .build();
        if (path.endsWith(AdminConstants.URI_SLASH_SUFFIX)) {
            ruleConditionDTO.setOperator(OperatorEnum.STARTS_WITH.getAlias());
        } else if (path.endsWith(AdminConstants.URI_SUFFIX)) {
            ruleConditionDTO.setOperator(OperatorEnum.PATH_PATTERN.getAlias());
        } else if (path.indexOf("*") > 1) {
            ruleConditionDTO.setOperator(OperatorEnum.MATCH.getAlias());
        } else {
            ruleConditionDTO.setOperator(OperatorEnum.EQ.getAlias());
        }
        ruleDTO.setRuleConditions(Collections.singletonList(ruleConditionDTO));
        return ruleDTO;
    }

    /**
     * adjustment such as '/aa/${xxx}/cc' replace to `/aa/`**`/cc` for client simpler annotation.
     * link: https://github.com/apache/shenyu/pull/3819
     * @param path the path
     * @return the replaced path
     */
    private String rewritePath(final String path) {
        if (path.contains(AdminConstants.URI_VARIABLE_SUFFIX)) {
            return path.replaceAll("(/\\{.*?})+", "/**");
        }
        return path;
    }
}
