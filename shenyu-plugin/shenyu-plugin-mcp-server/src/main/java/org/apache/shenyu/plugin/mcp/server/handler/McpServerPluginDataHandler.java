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

package org.apache.shenyu.plugin.mcp.server.handler;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.McpParameter;
import org.apache.shenyu.common.dto.convert.rule.impl.McpServerPluginRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpToolsManager;
import org.apache.shenyu.plugin.mcp.server.utils.JsonSchemaUtil;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * The type McpServer plugin data handler.
 */
public class McpServerPluginDataHandler implements PluginDataHandler {
    
    public static final Supplier<CommonHandleCache<String, McpServerPluginRuleHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);
    
    private final ShenyuMcpToolsManager shenyuMcpToolsManager;
    
    public McpServerPluginDataHandler(final ShenyuMcpToolsManager shenyuMcpToolsManager) {
        this.shenyuMcpToolsManager = shenyuMcpToolsManager;
    }
    
    @Override
    public void handlerSelector(final SelectorData selectorData) {
        if (Objects.isNull(selectorData) || Objects.isNull(selectorData.getId())) {
            return;
        }
        // the update is also need to clean, but there is no way to
        // distinguish between crate and update, so it is always clean
        MetaDataCache.getInstance().clean();
        if (!selectorData.getContinued()) {
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE), McpServerPluginRuleHandle.newInstance());
        }
    }
    
    @Override
    public void removeSelector(final SelectorData selectorData) {
        UpstreamCacheManager.getInstance().removeByKey(selectorData.getId());
        MetaDataCache.getInstance().clean();
        CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE));
    }
    
    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            McpServerPluginRuleHandle toolHandle = GsonUtils.getInstance().fromJson(s, McpServerPluginRuleHandle.class);
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), toolHandle);
            // the update is also need to clean, but there is no way to
            // distinguish between crate and update, so it is always clean
            MetaDataCache.getInstance().clean();
            
            List<McpParameter> parameters = toolHandle.getParameters();
            
            
            shenyuMcpToolsManager.addTool(
                    StringUtils.isBlank(toolHandle.getName()) ? ruleData.getName() : toolHandle.getName(),
                    toolHandle.getDescription(),
                    toolHandle.getRequestTemplate(),
                    // TODO : parameters
                    JsonSchemaUtil.emptySchema());
        });
    }
    
    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
            SpringBeanUtils.getInstance().getBean(ShenyuMcpToolsManager.class).removeTool(ruleData.getName());
        });
        MetaDataCache.getInstance().clean();
    }
    
    @Override
    public String pluginNamed() {
        return PluginEnum.MCP_SERVER.getName();
    }
    
}
