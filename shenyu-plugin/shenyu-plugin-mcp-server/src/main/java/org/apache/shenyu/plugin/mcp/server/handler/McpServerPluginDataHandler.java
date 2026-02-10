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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager;
import org.apache.shenyu.plugin.mcp.server.model.McpServerToolParameter;
import org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServer;
import org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServerTool;
import org.apache.shenyu.plugin.mcp.server.utils.JsonSchemaUtil;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * The type McpServer plugin data handler.
 */
public class McpServerPluginDataHandler implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, ShenyuMcpServer>> CACHED_SERVER = new BeanHolder<>(
            CommonHandleCache::new);

    public static final Supplier<CommonHandleCache<String, ShenyuMcpServerTool>> CACHED_TOOL = new BeanHolder<>(
            CommonHandleCache::new);

    private static final String DEFAULT_MESSAGE_ENDPOINT = "{\"messageEndpoint\":\"/message\"}";

    private static final String SLASH = "/";

    private static final String STAR = "/**";
    
    private static final String STREAMABLE_HTTP_PATH = "/streamablehttp";

    private final ShenyuMcpServerManager shenyuMcpServerManager;

    public McpServerPluginDataHandler(
            final ShenyuMcpServerManager shenyuMcpServerManager) {
        this.shenyuMcpServerManager = shenyuMcpServerManager;
    }

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        if (Objects.isNull(selectorData) || Objects.isNull(selectorData.getId())) {
            return;
        }

        if (CollectionUtils.isEmpty(selectorData.getConditionList())) {
            return;
        }

        String uri = extractSelectorUri(selectorData);
        if (StringUtils.isBlank(uri)) {
            return;
        String uri = extractSelectorUri(selectorData);
        if (StringUtils.isBlank(uri)) {
            // No valid URI condition found; do not cache or create transports.
            return;
        }
        String path = normalizeSelectorPath(uri);
        if (StringUtils.isBlank(path)) {
            // Normalization did not yield a usable path; abort handling.
            return;
        }
        if (StringUtils.isBlank(path)) {
            return;
        }
        ShenyuMcpServer shenyuMcpServer = GsonUtils.getInstance().fromJson(StringUtils.isBlank(selectorData.getHandle()) ? DEFAULT_MESSAGE_ENDPOINT : selectorData.getHandle(), ShenyuMcpServer.class);
        shenyuMcpServer.setPath(path);
        CACHED_SERVER.get().cachedHandle(
                selectorData.getId(),
                shenyuMcpServer);
        String messageEndpoint = shenyuMcpServer.getMessageEndpoint();
        // Get or create McpServer for this URI
        if (StringUtils.isNotBlank(path) && !shenyuMcpServerManager.hasMcpServer(path)) {
            shenyuMcpServerManager.getOrCreateMcpServerTransport(path, messageEndpoint);
        }
        if (StringUtils.isNotBlank(path)) {
            shenyuMcpServerManager.getOrCreateStreamableHttpTransport(path + STREAMABLE_HTTP_PATH);
        }

        // the update is also need to clean, but there is no way to
        // distinguish between crate and update, so it is always clean
        MetaDataCache.getInstance().clean();
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        if (Objects.isNull(selectorData) || Objects.isNull(selectorData.getId())) {
            return;
        }
        UpstreamCacheManager.getInstance().removeByKey(selectorData.getId());
        MetaDataCache.getInstance().clean();
        CACHED_TOOL.get().removeHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE));

        String path = normalizeSelectorPath(extractSelectorUri(selectorData));

        CACHED_SERVER.get().removeHandle(selectorData.getId());

        if (StringUtils.isNotBlank(path) && shenyuMcpServerManager.hasMcpServer(path)) {
            shenyuMcpServerManager.removeMcpServer(path);
        }
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        if (Objects.isNull(ruleData)) {
            return;
        }
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            ShenyuMcpServerTool mcpServerTool = GsonUtils.getInstance().fromJson(s, ShenyuMcpServerTool.class);
            CACHED_TOOL.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), mcpServerTool);
            // the update is also need to clean, but there is no way to
            // distinguish between crate and update, so it is always clean
            MetaDataCache.getInstance().clean();

            List<McpServerToolParameter> parameters = mcpServerTool.getParameters();

            // Create JSON schema from parameters
            String inputSchema = JsonSchemaUtil.createParameterSchema(parameters);
            ShenyuMcpServer server = CACHED_SERVER.get().obtainHandle(ruleData.getSelectorId());
            if (Objects.nonNull(server) && StringUtils.isNotBlank(server.getPath())) {
                shenyuMcpServerManager.addTool(server.getPath(),
                        StringUtils.isBlank(mcpServerTool.getName()) ? ruleData.getName()
                                : mcpServerTool.getName(),
                        mcpServerTool.getDescription(),
                        mcpServerTool.getRequestConfig(),
                        inputSchema);
            }
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        if (Objects.isNull(ruleData)) {
            return;
        }
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            CACHED_TOOL.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
            ShenyuMcpServer server = CACHED_SERVER.get().obtainHandle(ruleData.getSelectorId());
            if (Objects.nonNull(server) && StringUtils.isNotBlank(server.getPath())) {
                shenyuMcpServerManager.removeTool(server.getPath(), ruleData.getName());
            }
        });
        MetaDataCache.getInstance().clean();
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.MCP_SERVER.getName();
    }

    private String extractSelectorUri(final SelectorData selectorData) {
        if (Objects.isNull(selectorData) || CollectionUtils.isEmpty(selectorData.getConditionList())) {
            return null;
        }
        return selectorData.getConditionList().stream()
                .filter(condition -> Constants.URI.equals(condition.getParamType()))
                .map(ConditionData::getParamValue)
                .findFirst()
                .orElse(null);
    }

    private String normalizeSelectorPath(final String selectorUri) {
        if (StringUtils.isBlank(selectorUri)) {
            return selectorUri;
        }
        String path = StringUtils.removeEnd(selectorUri, STAR);
        path = StringUtils.removeEnd(path, SLASH);
        return StringUtils.defaultIfBlank(path, SLASH);
    }

}
