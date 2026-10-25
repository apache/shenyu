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
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * The type McpServer plugin data handler.
 * Handler mutations are serialized before calling the manager, whose tool registration blocks.
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

    private final ConcurrentMap<String, RuleData> rules = new ConcurrentHashMap<>();

    private final ConcurrentMap<String, ToolRegistration> registrations = new ConcurrentHashMap<>();

    public McpServerPluginDataHandler(
            final ShenyuMcpServerManager shenyuMcpServerManager) {
        this.shenyuMcpServerManager = shenyuMcpServerManager;
    }

    @Override
    public synchronized void handlerSelector(final SelectorData selectorData) {
        if (Objects.isNull(selectorData) || Objects.isNull(selectorData.getId())) {
            return;
        }

        if (CollectionUtils.isEmpty(selectorData.getConditionList())) {
            return;
        }

        String uri = extractSelectorUri(selectorData);
        if (StringUtils.isBlank(uri)) {
            return;
        }
        String path = normalizeSelectorPath(uri);
        if (StringUtils.isBlank(path)) {
            return;
        }
        ShenyuMcpServer shenyuMcpServer = GsonUtils.getInstance().fromJson(StringUtils.isBlank(selectorData.getHandle()) ? DEFAULT_MESSAGE_ENDPOINT : selectorData.getHandle(), ShenyuMcpServer.class);
        shenyuMcpServer.setPath(path);
        // when the selector moves to another path, close the server registered for the old
        // one, otherwise its routes and transports stay alive under the previous endpoint
        ShenyuMcpServer previousServer = CACHED_SERVER.get().obtainHandle(selectorData.getId());
        final boolean pathMoved = Objects.nonNull(previousServer) && StringUtils.isNotBlank(previousServer.getPath())
                && !previousServer.getPath().equals(path);
        if (pathMoved && shenyuMcpServerManager.hasMcpServer(previousServer.getPath())) {
            shenyuMcpServerManager.removeMcpServer(previousServer.getPath());
        }
        CACHED_SERVER.get().cachedHandle(
                selectorData.getId(),
                shenyuMcpServer);
        String messageEndpoint = shenyuMcpServer.getMessageEndpoint();
        final boolean serverCreated = !shenyuMcpServerManager.hasMcpServer(path);
        // Get or create McpServer for this URI
        if (serverCreated) {
            shenyuMcpServerManager.getOrCreateMcpServerTransport(path, messageEndpoint);
        }
        if (StringUtils.isNotBlank(path)) {
            shenyuMcpServerManager.getOrCreateStreamableHttpTransport(path + STREAMABLE_HTTP_PATH);
        }

        // Sync groups can arrive in either order after restart. Replay rules once their server exists.
        if (serverCreated || pathMoved) {
            rules.values().stream().filter(rule -> selectorData.getId().equals(rule.getSelectorId()))
                    .forEach(this::registerTool);
        }

        // the update is also need to clean, but there is no way to
        // distinguish between crate and update, so it is always clean
        MetaDataCache.getInstance().clean();
    }

    @Override
    public synchronized void removeSelector(final SelectorData selectorData) {
        if (Objects.isNull(selectorData) || Objects.isNull(selectorData.getId())) {
            return;
        }
        UpstreamCacheManager.getInstance().removeByKey(selectorData.getId());
        MetaDataCache.getInstance().clean();
        CACHED_TOOL.get().removeHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE));
        List<RuleData> removedRules = rules.values().stream().filter(rule -> selectorData.getId().equals(rule.getSelectorId()))
                .collect(Collectors.toList());
        removedRules.forEach(rule -> {
            rules.remove(rule.getId());
            registrations.remove(rule.getId());
            CACHED_TOOL.get().removeHandle(CacheKeyUtils.INST.getKey(rule));
        });

        String path = normalizeSelectorPath(extractSelectorUri(selectorData));

        CACHED_SERVER.get().removeHandle(selectorData.getId());

        if (StringUtils.isNotBlank(path) && shenyuMcpServerManager.hasMcpServer(path)) {
            shenyuMcpServerManager.removeMcpServer(path);
        }
    }

    @Override
    public synchronized void handlerRule(final RuleData ruleData) {
        if (Objects.isNull(ruleData)) {
            return;
        }
        if (Objects.nonNull(ruleData.getId())) {
            RuleData previous = rules.get(ruleData.getId());
            RuleData snapshot = RuleData.builder().id(ruleData.getId()).selectorId(ruleData.getSelectorId())
                    .name(ruleData.getName()).enabled(ruleData.getEnabled()).handle(ruleData.getHandle()).build();
            rules.put(snapshot.getId(), snapshot);
            // an enabled -> disabled update arrives here, not through removeRule, so the tool
            // registered while the rule was enabled has to be revoked explicitly
            if (isRegistered(previous) && Boolean.FALSE.equals(snapshot.getEnabled())) {
                unregisterTool(previous);
                MetaDataCache.getInstance().clean();
            }
            registerTool(snapshot);
            return;
        }
        registerTool(ruleData);
    }

    private boolean isRegistered(final RuleData ruleData) {
        return Objects.nonNull(ruleData) && !Boolean.FALSE.equals(ruleData.getEnabled());
    }

    private void registerTool(final RuleData ruleData) {
        if (Boolean.FALSE.equals(ruleData.getEnabled())) {
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
                String toolName = effectiveToolName(ruleData, mcpServerTool);
                revokeRenamedRegistration(ruleData.getId(), server.getPath(), toolName);
                shenyuMcpServerManager.addTool(server.getPath(), toolName,
                        mcpServerTool.getDescription(),
                        mcpServerTool.getRequestConfig(),
                        inputSchema);
                trackRegistration(ruleData, server.getPath(), toolName);
            }
        });
    }

    /**
     * Revoke the registration applied for this rule when its effective server path or tool name
     * changed, so a rename or a move does not leave the previous name registered next to the new
     * one. The applied identity is tracked separately from the newest desired rule definition.
     *
     * @param ruleId     the rule id
     * @param serverPath the server path the rule resolves to now
     * @param toolName   the effective tool name now
     */
    private void revokeRenamedRegistration(final String ruleId, final String serverPath, final String toolName) {
        if (Objects.isNull(ruleId)) {
            return;
        }
        ToolRegistration applied = registrations.get(ruleId);
        if (Objects.nonNull(applied) && !applied.hasIdentity(serverPath, toolName)) {
            shenyuMcpServerManager.removeTool(applied.getServerPath(), applied.getToolName());
        }
    }

    private void trackRegistration(final RuleData ruleData, final String serverPath, final String toolName) {
        if (Objects.nonNull(ruleData.getId()) && StringUtils.isNotBlank(toolName)) {
            registrations.put(ruleData.getId(), new ToolRegistration(ruleData.getSelectorId(), serverPath, toolName));
        }
    }

    private boolean removeTrackedRegistration(final String ruleId) {
        ToolRegistration applied = Objects.isNull(ruleId) ? null : registrations.remove(ruleId);
        if (Objects.isNull(applied)) {
            return false;
        }
        CACHED_TOOL.get().removeHandle(CacheKeyUtils.INST.getKey(applied.getSelectorId(), ruleId));
        if (StringUtils.isNotBlank(applied.getToolName())) {
            shenyuMcpServerManager.removeTool(applied.getServerPath(), applied.getToolName());
        }
        return true;
    }

    private String effectiveToolName(final RuleData ruleData, final ShenyuMcpServerTool tool) {
        return StringUtils.isBlank(tool.getName()) ? ruleData.getName() : tool.getName();
    }

    /**
     * Revoke whatever this rule has registered. The applied identity is authoritative because a
     * renamed or moved rule no longer matches what an incoming handle describes; the passed rule
     * data is only a fallback for rules that were never registered against a server.
     *
     * @param ruleData the rule to revoke
     */
    private void unregisterTool(final RuleData ruleData) {
        if (removeTrackedRegistration(ruleData.getId())) {
            return;
        }
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            CACHED_TOOL.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
            ShenyuMcpServerTool mcpServerTool = GsonUtils.getInstance().fromJson(s, ShenyuMcpServerTool.class);
            ShenyuMcpServer server = CACHED_SERVER.get().obtainHandle(ruleData.getSelectorId());
            if (Objects.nonNull(server) && StringUtils.isNotBlank(server.getPath())) {
                shenyuMcpServerManager.removeTool(server.getPath(), effectiveToolName(ruleData, mcpServerTool));
            }
        });
    }

    @Override
    public synchronized void removeRule(final RuleData ruleData) {
        if (Objects.isNull(ruleData)) {
            return;
        }
        if (Objects.nonNull(ruleData.getId())) {
            rules.remove(ruleData.getId());
        }
        unregisterTool(ruleData);
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

    /**
     * The registration this handler actually applied for one rule: the server path and tool name
     * present in the SDK server, kept apart from the newest desired rule definition so a later
     * update or deletion can tell what still has to be revoked.
     */
    private static final class ToolRegistration {

        private final String selectorId;

        private final String serverPath;

        private final String toolName;

        ToolRegistration(final String selectorId, final String serverPath, final String toolName) {
            this.selectorId = selectorId;
            this.serverPath = serverPath;
            this.toolName = toolName;
        }

        private String getSelectorId() {
            return selectorId;
        }

        private String getServerPath() {
            return serverPath;
        }

        private String getToolName() {
            return toolName;
        }

        private boolean hasIdentity(final String candidateServerPath, final String candidateToolName) {
            return Objects.equals(serverPath, candidateServerPath) && Objects.equals(toolName, candidateToolName);
        }
    }

}
