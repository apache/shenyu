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
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.PluginNameAdapter;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * mcp service register.
 */
@Service
public class ShenyuClientRegisterMcpServiceImpl extends AbstractShenyuClientRegisterServiceImpl {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuClientRegisterMcpServiceImpl.class);

    @Override
    protected String selectorHandler(final MetaDataRegisterDTO metaDataDTO) {
        return null;
    }

    @Override
    protected String ruleHandler() {
        return null;
    }

    @Override
    protected void registerMetadata(final MetaDataRegisterDTO metaDataDTO) {
        MetaDataService metaDataService = getMetaDataService();
        if (LOG.isDebugEnabled()) {
            LOG.debug("mcp register metadata:{}", GsonUtils.getInstance().toJson(metaDataDTO));
        }
        MetaDataDO exist = metaDataService.findByPathAndNamespaceId(metaDataDTO.getPath(), metaDataDTO.getNamespaceId());
        metaDataService.saveOrUpdateMetaData(exist, metaDataDTO);
    }

    @Override
    protected String buildHandle(final List<URIRegisterDTO> uriList, final SelectorDO selectorDO) {
        return null;
    }

    @Override
    public String rpcType() {
        return RpcTypeEnum.MCP.getName();
    }

    public void registerMcpTools(final McpToolsRegisterDTO mcpToolsRegisterDTO) {

        MetaDataRegisterDTO metaDataRegisterDTO = mcpToolsRegisterDTO.getMetaDataRegisterDTO();

        String namespaceId = StringUtils.defaultIfEmpty(mcpToolsRegisterDTO.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID);
        String pluginName = PluginNameAdapter.rpcTypeAdapter(rpcType());
        this.checkNamespacePluginRel(namespaceId, pluginName);
        mcpToolsRegisterDTO.setNamespaceId(namespaceId);
        //handler plugin selector
        String selectorHandler = "";
        String selectorId = getSelectorService().registerDefault(metaDataRegisterDTO, PluginNameAdapter.rpcTypeAdapter(rpcType()), selectorHandler);
        //handler selector rule
        String ruleHandler = mcpToolsRegisterDTO.getMcpConfig();
        RuleDTO ruleDTO = buildMcpRuleDTO(selectorId, metaDataRegisterDTO, ruleHandler);
        getRuleService().registerDefault(ruleDTO);
        //handler register metadata
        registerMetadata(metaDataRegisterDTO);
        //handler context path
        String contextPath = metaDataRegisterDTO.getContextPath();
        if (StringUtils.isNotEmpty(contextPath)) {
            registerContextPath(metaDataRegisterDTO);
        }
    }

    private RuleDTO buildMcpRuleDTO(final String selectorId, final MetaDataRegisterDTO metaDataRegisterDTO, final String ruleHandler) {
        RuleDTO ruleDTO = RuleDTO.builder()
                .selectorId(selectorId)
                .matchMode(MatchModeEnum.AND.getCode())
                .name(metaDataRegisterDTO.getRuleName())
                .enabled(Boolean.TRUE)
                .loged(Boolean.TRUE)
                .matchRestful(Boolean.FALSE)
                .sort(1)
                .handle(ruleHandler)
                .namespaceId(metaDataRegisterDTO.getNamespaceId())
                .build();
        RuleConditionDTO ruleConditionDTO = RuleConditionDTO.builder()
                .paramType(ParamTypeEnum.URI.getName())
                .paramName("/")
                .paramValue("/**")
                .build();

        ruleConditionDTO.setOperator(OperatorEnum.MATCH.getAlias());
        ruleDTO.setRuleConditions(Collections.singletonList(ruleConditionDTO));
        return ruleDTO;
    }

}
