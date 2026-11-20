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

package org.apache.shenyu.admin.service;

import org.apache.commons.lang3.StringUtils;
import org.apache.ibatis.exceptions.TooManyResultsException;
import org.apache.shenyu.admin.discovery.DiscoveryLevel;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryRelDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.SelectorExportDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.model.vo.PluginHandleVO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shenyu.common.constant.ExportImportConstants;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfigValidator;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.JsonUtils;
import org.mybatis.spring.MyBatisSystemException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Export only explicitly selected divide routes and their static HTTP dependencies.
 * Other plugins, metadata and credentials are intentionally outside this archive.
 */
@Service
public class SelectiveConfigsExportService {

    private static final int MAX_OBJECTS = 1000;

    private static final int MAX_BYTES = 10 * 1024 * 1024;

    private final SelectorMapper selectorMapper;

    private final RuleMapper ruleMapper;

    private final SelectorService selectorService;

    private final RuleService ruleService;

    private final PluginService pluginService;

    private final NamespacePluginService namespacePluginService;

    private final PluginHandleService pluginHandleService;

    private final DiscoveryMapper discoveryMapper;

    private final DiscoveryHandlerMapper handlerMapper;

    private final DiscoveryRelMapper relMapper;

    private final DiscoveryUpstreamMapper upstreamMapper;

    public SelectiveConfigsExportService(final SelectorMapper selectorMapper, final RuleMapper ruleMapper,
                                        final SelectorService selectorService, final RuleService ruleService,
                                        final PluginService pluginService, final NamespacePluginService namespacePluginService,
                                        final PluginHandleService pluginHandleService, final DiscoveryMapper discoveryMapper, final DiscoveryHandlerMapper handlerMapper,
                                        final DiscoveryRelMapper relMapper, final DiscoveryUpstreamMapper upstreamMapper) {
        this.selectorMapper = selectorMapper;
        this.ruleMapper = ruleMapper;
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.pluginService = pluginService;
        this.namespacePluginService = namespacePluginService;
        this.pluginHandleService = pluginHandleService;
        this.discoveryMapper = discoveryMapper;
        this.handlerMapper = handlerMapper;
        this.relMapper = relMapper;
        this.upstreamMapper = upstreamMapper;
    }

    /**
     * Build an import-compatible ZIP without changing persisted configuration.
     *
     * @param request source namespace and selected IDs (at most 100)
     * @return ZIP bytes
     */
    @Transactional(readOnly = true)
    public byte[] export(final SelectorExportDTO request) {
        require(Objects.nonNull(request) && StringUtils.isNotBlank(request.getNamespace()) && request.getNamespace().length() <= 128,
                "A source namespace is required (at most 128 characters)");
        List<String> ids = request.getSelectorIds();
        require(Objects.nonNull(ids) && !ids.isEmpty() && ids.size() <= 100, "Select between 1 and 100 selectors");
        require(ids.stream().allMatch(id -> StringUtils.isNotBlank(id) && id.length() <= 128), "Invalid selector ID");
        require(new HashSet<>(ids).size() == ids.size(), "Duplicate selector IDs are not supported");
        String namespace = request.getNamespace();
        // Resolve namespace membership before loading any selected configuration.
        for (String id : ids) {
            require(namespace.equals(selectorMapper.selectNamespaceIdById(id)), "Every selector must belong to the source namespace");
        }
        List<SelectorVO> selectors = new ArrayList<>();
        List<RuleVO> rules = new ArrayList<>();
        List<DiscoveryDTO> discoveries = new ArrayList<>();
        List<DiscoveryUpstreamDTO> upstreams = new ArrayList<>();
        Set<String> discoveryNames = new HashSet<>();
        PluginVO template = null;
        int conditionCount = 0;
        for (String id : ids) {
            // Detail services enforce the existing selector/rule data permissions.
            SelectorVO selector = readSelector(id);
            require(Objects.nonNull(selector) && namespace.equals(selector.getNamespaceId()), "Selector changed during export; retry");
            PluginVO plugin = pluginService.findById(selector.getPluginId());
            require(Objects.nonNull(plugin) && "divide".equals(plugin.getName()), "Only divide HTTP selectors are supported");
            require(Objects.isNull(template) || template.getId().equals(plugin.getId()), "Ambiguous divide plugin templates");
            template = plugin;
            collectDiscovery(namespace, id, discoveries, upstreams, discoveryNames);
            RuleQuery query = new RuleQuery(id, null, null);
            require(rules.size() + ruleMapper.countByQuery(query) <= MAX_OBJECTS, "Too many rules in selection (maximum 1000 objects)");
            for (RuleDO row : ruleMapper.findBySelectorId(id)) {
                require(namespace.equals(row.getNamespaceId()), "Rule namespace differs from its selector");
                RuleVO rule = ruleService.findById(row.getId());
                require(Objects.nonNull(rule) && id.equals(rule.getSelectorId()), "Rule changed during export; retry");
                // Canary extension dictionaries are global and are validated before dictionary import.
                require(StringUtils.isBlank(rule.getHandle()) || Objects.isNull(CanaryConfigValidator.parseHandle(rule.getHandle()).getCanary()),
                        "Canary rule configurations are not supported by selective export");
                conditionCount += Objects.isNull(rule.getRuleConditions()) ? 0 : rule.getRuleConditions().size();
                rules.add(rule);
            }
            conditionCount += Objects.isNull(selector.getSelectorConditions()) ? 0 : selector.getSelectorConditions().size();
            selector.setNamespaceId(null);
            // Discovery uses its own import section; avoid exporting duplicate nested representations.
            selector.setDiscoveryVO(null);
            selector.setDiscoveryHandler(null);
            selector.setDiscoveryUpstreams(null);
            selector.setSelectorRules(null);
            selectors.add(selector);
            require(selectors.size() + rules.size() + conditionCount + discoveries.size() * 3 + upstreams.size() + 2 <= MAX_OBJECTS,
                    "Selection exceeds maximum 1000 configuration objects");
        }
        NamespacePluginVO namespacePlugin = namespacePluginService.findByNamespaceIdAndPluginId(namespace, template.getId());
        require(Objects.nonNull(namespacePlugin), "Missing source namespace divide configuration");
        require(Objects.isNull(namespacePlugin.getPluginJar()) || namespacePlugin.getPluginJar().length == 0, "Custom plugin JARs are not supported");
        namespacePlugin.setNamespaceId(null);
        List<ZipUtil.ZipItem> items = new ArrayList<>();
        addPluginSchema(template.getId(), selectors.size() + rules.size() + conditionCount + discoveries.size() * 3 + upstreams.size() + 2, items);
        items.add(new ZipUtil.ZipItem(ExportImportConstants.PLUGIN_TEMPLATE_JSON, JsonUtils.toJson(List.of(template))));
        items.add(new ZipUtil.ZipItem(ExportImportConstants.NAMESPACE_PLUGIN_JSON, JsonUtils.toJson(List.of(namespacePlugin))));
        items.add(new ZipUtil.ZipItem(ExportImportConstants.SELECTOR_JSON, JsonUtils.toJson(selectors)));
        items.add(new ZipUtil.ZipItem(ExportImportConstants.RULE_JSON, JsonUtils.toJson(rules)));
        if (!discoveries.isEmpty()) {
            items.add(new ZipUtil.ZipItem(ExportImportConstants.DISCOVERY_JSON, JsonUtils.toJson(discoveries)));
            items.add(new ZipUtil.ZipItem(ExportImportConstants.DISCOVERY_UPSTREAM_JSON, JsonUtils.toJson(upstreams)));
        }
        long size = items.stream().mapToLong(item -> item.getItemData().getBytes(StandardCharsets.UTF_8).length).sum();
        require(size <= MAX_BYTES, "Selection exceeds maximum 10 MiB uncompressed export size");
        return ZipUtil.zip(items);
    }

    private SelectorVO readSelector(final String id) {
        try {
            return selectorService.findById(id);
        } catch (MyBatisSystemException ex) {
            if (ex.getCause() instanceof TooManyResultsException) {
                throw new ShenyuException("Ambiguous selector discovery bindings are not supported");
            }
            throw ex;
        }
    }

    private void addPluginSchema(final String pluginId, final int objectCount, final List<ZipUtil.ZipItem> items) {
        List<PluginHandleVO> handles = pluginHandleService.listAllDataByPluginIds(List.of(pluginId));
        List<ShenyuDictVO> options = new ArrayList<>(handles.stream().filter(handle -> Objects.nonNull(handle.getDictOptions()))
                .flatMap(handle -> handle.getDictOptions().stream())
                .collect(Collectors.toMap(ShenyuDictVO::getId, option -> option, (first, second) -> first)).values());
        require(objectCount + handles.size() + options.size() <= MAX_OBJECTS, "Selection exceeds maximum 1000 configuration objects");
        // These records have business keys, but no archive references to their source IDs.
        handles.forEach(handle -> handle.setId(null));
        options.forEach(option -> option.setId(null));
        if (!handles.isEmpty()) {
            items.add(new ZipUtil.ZipItem(ExportImportConstants.PLUGIN_HANDLE_JSON, JsonUtils.toJson(handles)));
        }
        if (!options.isEmpty()) {
            items.add(new ZipUtil.ZipItem(ExportImportConstants.DICT_JSON, JsonUtils.toJson(options)));
        }
    }

    private void collectDiscovery(final String namespace, final String selectorId, final List<DiscoveryDTO> discoveries,
                                  final List<DiscoveryUpstreamDTO> upstreams, final Set<String> names) {
        DiscoveryHandlerDO handler = handlerMapper.selectBySelectorId(selectorId);
        require(Objects.nonNull(handler), "A selector-local discovery binding is required; inline-only upstream handles are not supported");
        DiscoveryDO discovery = discoveryMapper.selectById(handler.getDiscoveryId());
        require(Objects.nonNull(discovery) && namespace.equals(discovery.getNamespaceId()) && "divide".equals(discovery.getPluginName())
                        && "local".equals(discovery.getDiscoveryType()) && DiscoveryLevel.SELECTOR.getCode().equals(discovery.getDiscoveryLevel()),
                "Only selector-local divide discovery is supported; external, shared or RPC discovery cannot be exported");
        List<DiscoveryHandlerDO> handlers = handlerMapper.selectByDiscoveryId(discovery.getId());
        require(handlers.size() == 1 && handler.getId().equals(handlers.get(0).getId()), "Shared discovery handlers are not supported");
        // The selector detail join omits handler props; export the complete handler record.
        handler = handlers.get(0);
        List<DiscoveryRelDO> relations = relMapper.selectByDiscoveryHandlerIds(List.of(handler.getId()));
        require(relations.size() == 1 && selectorId.equals(relations.get(0).getSelectorId())
                        && StringUtils.isBlank(relations.get(0).getProxySelectorId()) && "divide".equals(relations.get(0).getPluginName()),
                "Ambiguous discovery relations are not supported");
        require(names.add(discovery.getDiscoveryName()), "Duplicate discovery names are not supported");
        List<DiscoveryUpstreamDO> rows = upstreamMapper.selectByDiscoveryHandlerId(handler.getId());
        require(!rows.isEmpty() && upstreams.size() + rows.size() <= MAX_OBJECTS, "Missing or too many local upstreams");
        for (DiscoveryUpstreamDO row : rows) {
            require(namespace.equals(row.getNamespaceId()) && ("http".equals(row.getProtocol()) || "https".equals(row.getProtocol())),
                    "Only HTTP/HTTPS upstreams in the source namespace are supported");
            upstreams.add(DiscoveryTransfer.INSTANCE.mapToDTO(row));
        }
        DiscoveryDTO dto = DiscoveryTransfer.INSTANCE.mapToDTO(discovery);
        dto.setNamespaceId(null);
        dto.setDiscoveryHandler(DiscoveryTransfer.INSTANCE.mapToDTO(handler));
        DiscoveryRelDTO relation = new DiscoveryRelDTO();
        relation.setId(relations.get(0).getId());
        relation.setDiscoveryHandlerId(handler.getId());
        relation.setPluginName("divide");
        relation.setSelectorId(selectorId);
        relation.setProxySelectorId(relations.get(0).getProxySelectorId());
        dto.setDiscoveryRel(relation);
        discoveries.add(dto);
    }

    private void require(final boolean condition, final String message) {
        if (!condition) {
            throw new ShenyuException(message);
        }
    }
}
