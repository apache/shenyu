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

package org.apache.shenyu.admin.service.impl;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.AppAuthDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.AppAuthVO;
import org.apache.shenyu.admin.model.vo.DiscoveryUpstreamVO;
import org.apache.shenyu.admin.model.vo.DiscoveryVO;
import org.apache.shenyu.admin.model.vo.MetaDataVO;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.model.vo.PluginHandleVO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.ConfigsService;
import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.NamespacePluginService;
import org.apache.shenyu.admin.service.PluginHandleService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.service.configs.ConfigsExportImportHandler;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shenyu.admin.utils.ZipUtil.ZipItem;
import org.apache.shenyu.common.constant.ExportImportConstants;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.ListUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ConfigsService}.
 */
@Service
public class ConfigsServiceImpl implements ConfigsService {

    private static final Logger LOG = LoggerFactory.getLogger(ConfigsServiceImpl.class);

    /**
     * The AppAuth service.
     */
    private final AppAuthService appAuthService;

    /**
     * The Plugin service.
     */
    private final PluginService pluginService;

    /**
     * The Namespace Plugin service.
     */
    private final NamespacePluginService namespacePluginService;

    /**
     * The Plugin Handle service.
     */
    private final PluginHandleService pluginHandleService;

    /**
     * The Selector service.
     */
    private final SelectorService selectorService;

    /**
     * The Rule service.
     */
    private final RuleService ruleService;

    /**
     * The Metadata service.
     */
    private final MetaDataService metaDataService;

    /**
     * The Dict service.
     */
    private final ShenyuDictService shenyuDictService;

    /**
     * The ProxySelector service.
     */
    private final ProxySelectorService proxySelectorService;

    /**
     * The discovery service.
     */
    private final DiscoveryService discoveryService;

    /**
     * The discovery upstream service.
     */
    private final DiscoveryUpstreamService discoveryUpstreamService;

    /**
     * The configs export import handlers.
     */
    private final List<ConfigsExportImportHandler> configsExportImportHandlers;

    public ConfigsServiceImpl(final AppAuthService appAuthService,
                                         final PluginService pluginService,
                                         final NamespacePluginService namespacePluginService,
                                         final PluginHandleService pluginHandleService,
                                         final SelectorService selectorService,
                                         final RuleService ruleService,
                                         final MetaDataService metaDataService,
                                         final ShenyuDictService shenyuDictService,
                                         final ProxySelectorService proxySelectorService,
                                         final DiscoveryService discoveryService,
                                         final DiscoveryUpstreamService discoveryUpstreamService,
                                         final List<ConfigsExportImportHandler> configsExportImportHandlers) {
        this.appAuthService = appAuthService;
        this.pluginService = pluginService;
        this.namespacePluginService = namespacePluginService;
        this.pluginHandleService = pluginHandleService;
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.metaDataService = metaDataService;
        this.shenyuDictService = shenyuDictService;
        this.proxySelectorService = proxySelectorService;
        this.discoveryService = discoveryService;
        this.discoveryUpstreamService = discoveryUpstreamService;
        this.configsExportImportHandlers = configsExportImportHandlers.stream()
                .sorted(Comparator.comparingInt(c -> c.configsEnum().getImportOrder())).toList();
    }

    @Override
    public ShenyuAdminResult configsExport() {
        List<ZipUtil.ZipItem> zipItemList = Lists.newArrayList();
        exportAuthData(zipItemList);

        exportMetadata(zipItemList);

        exportPluginData(zipItemList);

        exportSelectorData(zipItemList);

        exportRuleData(zipItemList);

        exportDictData(zipItemList);

        exportProxySelectorData(zipItemList);

        exportDiscoveryData(zipItemList);

        exportDiscoveryUpstreamData(zipItemList);

        return ShenyuAdminResult.success(ZipUtil.zip(zipItemList));
    }
    
    @Override
    public ShenyuAdminResult configsExport(final String namespace) {
        List<ZipUtil.ZipItem> zipItemList = Lists.newArrayList();

        for (ConfigsExportImportHandler configsExportImportHandler : configsExportImportHandlers) {
            configsExportImportHandler.configsExport(namespace)
                    .ifPresent(data -> zipItemList.add(new ZipUtil.ZipItem(configsExportImportHandler.configsEnum().getConfigName(), data)));
        }

        return ShenyuAdminResult.success(ZipUtil.zip(zipItemList));
    }
    
    private void exportPluginHandleData(final List<ZipItem> zipItemList) {
        List<PluginHandleVO> pluginHandleDataList = pluginHandleService.listAllData();
        if (CollectionUtils.isNotEmpty(pluginHandleDataList)) {
            zipItemList.add(new ZipItem(ExportImportConstants.PLUGIN_HANDLE_JSON, JsonUtils.toJson(pluginHandleDataList)));
        }
    }
    
    private void exportDiscoveryUpstreamData(final List<ZipUtil.ZipItem> zipItemList) {
        List<DiscoveryUpstreamVO> discoveryUpstreamList = discoveryUpstreamService.listAllData();
        if (CollectionUtils.isNotEmpty(discoveryUpstreamList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.DISCOVERY_UPSTREAM_JSON, JsonUtils.toJson(discoveryUpstreamList)));
        }
    }
    
    private void exportDiscoveryUpstreamData(final String namespace, final List<ZipUtil.ZipItem> zipItemList) {
        List<DiscoveryUpstreamVO> discoveryUpstreamList = discoveryUpstreamService.listAllDataByNamespaceId(namespace);
        if (CollectionUtils.isNotEmpty(discoveryUpstreamList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.DISCOVERY_UPSTREAM_JSON, JsonUtils.toJson(discoveryUpstreamList)));
        }
    }

    private void exportDiscoveryData(final List<ZipUtil.ZipItem> zipItemList) {
        List<DiscoveryVO> discoveryList = discoveryService.listAllData();
        if (CollectionUtils.isNotEmpty(discoveryList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.DISCOVERY_JSON, JsonUtils.toJson(discoveryList)));
        }
    }

    private void exportDiscoveryData(final String namespace, final List<ZipUtil.ZipItem> zipItemList) {
        List<DiscoveryVO> discoveryList = discoveryService.listAllDataByNamespaceId(namespace);
        if (CollectionUtils.isNotEmpty(discoveryList)) {
            discoveryList.forEach(discoveryVO -> discoveryVO.setNamespaceId(null));
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.DISCOVERY_JSON, JsonUtils.toJson(discoveryList)));
        }
    }

    private void exportProxySelectorData(final List<ZipUtil.ZipItem> zipItemList) {
        List<ProxySelectorData> proxySelectorDataList = proxySelectorService.listAll();
        if (CollectionUtils.isNotEmpty(proxySelectorDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.PROXY_SELECTOR_JSON, JsonUtils.toJson(proxySelectorDataList)));
        }
    }

    private void exportProxySelectorData(final String namespace, final List<ZipUtil.ZipItem> zipItemList) {
        List<ProxySelectorData> proxySelectorDataList = proxySelectorService.listAllByNamespaceId(namespace);
        if (CollectionUtils.isNotEmpty(proxySelectorDataList)) {
            proxySelectorDataList.forEach(proxySelectorData -> proxySelectorData.setNamespaceId(null));
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.PROXY_SELECTOR_JSON, JsonUtils.toJson(proxySelectorDataList)));
        }
    }

    private void exportDictData(final List<ZipUtil.ZipItem> zipItemList) {
        List<ShenyuDictVO> dictDataList = shenyuDictService.listAllData();
        if (CollectionUtils.isNotEmpty(dictDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.DICT_JSON, JsonUtils.toJson(dictDataList)));
        }
    }

    private void exportRuleData(final List<ZipUtil.ZipItem> zipItemList) {
        List<RuleVO> ruleDataList = ruleService.listAllData();
        if (CollectionUtils.isNotEmpty(ruleDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.RULE_JSON, JsonUtils.toJson(ruleDataList)));
        }
    }

    private void exportRuleData(final String namespace, final List<ZipUtil.ZipItem> zipItemList) {
        List<RuleVO> ruleDataList = ruleService.listAllDataByNamespaceId(namespace);
        if (CollectionUtils.isNotEmpty(ruleDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.RULE_JSON, JsonUtils.toJson(ruleDataList)));
        }
    }

    private void exportSelectorData(final List<ZipUtil.ZipItem> zipItemList) {
        List<SelectorVO> selectorDataList = selectorService.listAllData();
        if (CollectionUtils.isNotEmpty(selectorDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.SELECTOR_JSON, JsonUtils.toJson(selectorDataList)));
        }
    }

    private void exportSelectorData(final String namespace, final List<ZipUtil.ZipItem> zipItemList) {
        List<SelectorVO> selectorDataList = selectorService.listAllDataByNamespaceId(namespace);
        if (CollectionUtils.isNotEmpty(selectorDataList)) {
            selectorDataList.forEach(selectorVO -> selectorVO.setNamespaceId(null));
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.SELECTOR_JSON, JsonUtils.toJson(selectorDataList)));
        }
    }

    private void exportPluginData(final List<ZipUtil.ZipItem> zipItemList) {
        List<PluginVO> pluginDataList = pluginService.listAllData();
        if (CollectionUtils.isNotEmpty(pluginDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.PLUGIN_JSON, JsonUtils.toJson(pluginDataList)));
        }
    }

    private void exportNamespacePluginData(final String namespace, final List<ZipUtil.ZipItem> zipItemList) {
        List<NamespacePluginVO> namespacePluginVOList = namespacePluginService.listAllData(namespace);
        if (CollectionUtils.isNotEmpty(namespacePluginVOList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.NAMESPACE_PLUGIN_JSON, JsonUtils.toJson(namespacePluginVOList)));
        }
    }
    
    private void exportPluginTemplateData(final List<ZipUtil.ZipItem> zipItemList) {
        List<PluginVO> pluginDataList = pluginService.listAllData();
        if (CollectionUtils.isNotEmpty(pluginDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.PLUGIN_TEMPLATE_JSON, JsonUtils.toJson(pluginDataList)));
        }
    }

    private void exportMetadata(final List<ZipUtil.ZipItem> zipItemList) {
        List<MetaDataVO> metaDataList = metaDataService.listAllData();
        if (CollectionUtils.isNotEmpty(metaDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.META_JSON, JsonUtils.toJson(metaDataList)));
        }
    }

    private void exportMetadata(final String namespace, final List<ZipUtil.ZipItem> zipItemList) {
        List<MetaDataVO> metaDataList = metaDataService.listAllDataByNamespaceId(namespace);
        if (CollectionUtils.isNotEmpty(metaDataList)) {
            metaDataList.forEach(metaDataVO -> metaDataVO.setNamespaceId(null));
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.META_JSON, JsonUtils.toJson(metaDataList)));
        }
    }

    private void exportAuthData(final List<ZipUtil.ZipItem> zipItemList) {
        List<AppAuthVO> authDataList = appAuthService.listAllData();
        if (CollectionUtils.isNotEmpty(authDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.AUTH_JSON, JsonUtils.toJson(authDataList)));
        }
    }

    private void exportAuthData(final String namespace, final List<ZipUtil.ZipItem> zipItemList) {
        List<AppAuthVO> authDataList = appAuthService.listAllDataByNamespace(namespace);
        if (CollectionUtils.isNotEmpty(authDataList)) {
            authDataList.forEach(appAuthVO -> appAuthVO.setNamespaceId(null));
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.AUTH_JSON, JsonUtils.toJson(authDataList)));
        }
    }

    @Override
    public ShenyuAdminResult configsImport(final byte[] source) {
        ZipUtil.UnZipResult unZipResult = ZipUtil.unzip(source);
        List<ZipUtil.ZipItem> zipItemList = unZipResult.getZipItemList();
        if (CollectionUtils.isEmpty(zipItemList)) {
            LOG.info("import file is empty");
            return ShenyuAdminResult.success();
        }
        Map<String, Object> result = Maps.newHashMap();
        for (ZipUtil.ZipItem zipItem : zipItemList) {
            switch (zipItem.getItemName()) {
                case ExportImportConstants.AUTH_JSON:
                    importAuthData(result, zipItem);
                    break;
                case ExportImportConstants.META_JSON:
                    importMetaData(result, zipItem);
                    break;
                case ExportImportConstants.PLUGIN_JSON:
                    importPluginData(result, zipItem);
                    break;
                case ExportImportConstants.SELECTOR_JSON:
                    importSelectorData(result, zipItem);
                    break;
                case ExportImportConstants.RULE_JSON:
                    importRuleData(result, zipItem);
                    break;
                case ExportImportConstants.DICT_JSON:
                    importDictData(result, zipItem);
                    break;
                case ExportImportConstants.PROXY_SELECTOR_JSON:
                    importProxySelectorData(result, zipItem);
                    break;
                case ExportImportConstants.DISCOVERY_JSON:
                    importDiscoveryData(result, zipItem);
                    break;
                case ExportImportConstants.DISCOVERY_UPSTREAM_JSON:
                    importDiscoveryUpstreamData(result, zipItem);
                    break;
                default:
                    break;
            }
        }
        return ShenyuAdminResult.success(result);
    }
    
    @Override
    public ShenyuAdminResult configsImport(final String namespace, final byte[] source) {
        ZipUtil.UnZipResult unZipResult = ZipUtil.unzip(source);
        List<ZipUtil.ZipItem> zipItemList = unZipResult.getZipItemList();
        if (CollectionUtils.isEmpty(zipItemList)) {
            LOG.info("import file is empty");
            return ShenyuAdminResult.success();
        }
        ConfigsImportContext context = new ConfigsImportContext();
        Map<String, ZipUtil.ZipItem> zipItemNameMap = ListUtil.toMap(zipItemList, ZipUtil.ZipItem::getItemName);
        for (ConfigsExportImportHandler configsExportImportHandler : configsExportImportHandlers) {
            ZipUtil.ZipItem zipItem = zipItemNameMap.get(configsExportImportHandler.configsEnum().getConfigName());
            if (Objects.nonNull(zipItem) && StringUtils.isNoneBlank(zipItem.getItemData())) {
                configsExportImportHandler.configsImport(namespace, zipItem.getItemData(), context);
            }
        }
        return ShenyuAdminResult.success(context.getResult());
    }
    
    private void importDiscoveryUpstreamData(final Map<String, Object> result, final ZipUtil.ZipItem zipItem) {
        String discoveryUpstreamJson = zipItem.getItemData();
        if (StringUtils.isNotEmpty(discoveryUpstreamJson)) {
            List<DiscoveryUpstreamDTO> discoveryUpstreamList = GsonUtils.getInstance().fromList(discoveryUpstreamJson, DiscoveryUpstreamDTO.class);
            ConfigImportResult configImportResult = discoveryUpstreamService.importData(discoveryUpstreamList);
            result.put(ExportImportConstants.DISCOVERY_UPSTREAM_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                result.put(ExportImportConstants.DISCOVERY_UPSTREAM_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
            }
        }
    }

    private void importDiscoveryData(final Map<String, Object> result, final ZipUtil.ZipItem zipItem) {
        String discoveryJson = zipItem.getItemData();
        if (StringUtils.isNotEmpty(discoveryJson)) {
            List<DiscoveryDTO> discoveryList = GsonUtils.getInstance().fromList(discoveryJson, DiscoveryDTO.class);
            ConfigImportResult configImportResult = discoveryService.importData(discoveryList);
            result.put(ExportImportConstants.DISCOVERY_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                result.put(ExportImportConstants.DISCOVERY_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
            }
        }
    }

    private void importProxySelectorData(final Map<String, Object> result, final ZipUtil.ZipItem zipItem) {
        String proxySelectorJson = zipItem.getItemData();
        if (StringUtils.isNotEmpty(proxySelectorJson)) {
            List<ProxySelectorData> proxySelectorList = GsonUtils.getInstance().fromList(proxySelectorJson, ProxySelectorData.class);
            ConfigImportResult configImportResult = proxySelectorService.importData(proxySelectorList);
            result.put(ExportImportConstants.PROXY_SELECTOR_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                result.put(ExportImportConstants.PROXY_SELECTOR_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
            }
        }
    }

    private void importDictData(final Map<String, Object> result, final ZipUtil.ZipItem zipItem) {
        String dictJson = zipItem.getItemData();
        if (StringUtils.isNotEmpty(dictJson)) {
            List<ShenyuDictDTO> dictList = GsonUtils.getInstance().fromList(dictJson, ShenyuDictDTO.class);
            ConfigImportResult configImportResult = shenyuDictService.importData(dictList);
            result.put(ExportImportConstants.DICT_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                result.put(ExportImportConstants.DICT_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
            }
        }
    }

    private void importRuleData(final Map<String, Object> result, final ZipUtil.ZipItem zipItem) {
        String ruleJson = zipItem.getItemData();
        if (StringUtils.isNotEmpty(ruleJson)) {
            List<RuleDTO> ruleList = GsonUtils.getInstance().fromList(ruleJson, RuleDTO.class);
            ConfigImportResult configImportResult = ruleService.importData(ruleList);
            result.put(ExportImportConstants.RULE_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                result.put(ExportImportConstants.RULE_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
            }
        }
    }

    private void importSelectorData(final Map<String, Object> result, final ZipUtil.ZipItem zipItem) {
        String selectorJson = zipItem.getItemData();
        if (StringUtils.isNotEmpty(selectorJson)) {
            List<SelectorDTO> selectorList = GsonUtils.getInstance().fromList(selectorJson, SelectorDTO.class);
            ConfigImportResult configImportResult = selectorService.importData(selectorList);
            result.put(ExportImportConstants.SELECTOR_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                result.put(ExportImportConstants.SELECTOR_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
            }
        }
    }

    private void importPluginData(final Map<String, Object> result, final ZipUtil.ZipItem zipItem) {
        String pluginJson = zipItem.getItemData();
        if (StringUtils.isNotEmpty(pluginJson)) {
            List<PluginDTO> pluginList = GsonUtils.getInstance().fromList(pluginJson, PluginDTO.class);
            ConfigImportResult configImportResult = pluginService.importData(pluginList, null);
            result.put(ExportImportConstants.PLUGIN_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                result.put(ExportImportConstants.PLUGIN_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
            }
        }
    }

    private void importMetaData(final Map<String, Object> result, final ZipUtil.ZipItem zipItem) {
        String metaJson = zipItem.getItemData();
        if (StringUtils.isNotEmpty(metaJson)) {
            List<MetaDataDTO> metaDataList = GsonUtils.getInstance().fromList(metaJson, MetaDataDTO.class);
            ConfigImportResult configImportResult = metaDataService.importData(metaDataList);
            result.put(ExportImportConstants.META_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                result.put(ExportImportConstants.META_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
            }
        }
    }

    private void importAuthData(final Map<String, Object> result, final ZipUtil.ZipItem zipItem) {
        String authJson = zipItem.getItemData();
        if (StringUtils.isNotEmpty(authJson)) {
            List<AppAuthDTO> authDataList = GsonUtils.getInstance().fromList(authJson, AppAuthDTO.class);
            ConfigImportResult configImportResult = appAuthService.importData(authDataList);
            result.put(ExportImportConstants.AUTH_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                result.put(ExportImportConstants.AUTH_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
            }
        }
    }
}
