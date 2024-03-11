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

package org.apache.shenyu.admin.controller;

import com.alibaba.nacos.common.utils.DateFormatUtils;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
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
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shenyu.common.constant.ExportImportConstants;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * this is configs controller.
 */
@RestApi("/configs")
public class ConfigsExportImportController {

    private static final Logger LOG = LoggerFactory.getLogger(ConfigsExportImportController.class);

    /**
     * The AppAuth service.
     */
    private final AppAuthService appAuthService;

    /**
     * The Plugin service.
     */
    private final PluginService pluginService;

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

    public ConfigsExportImportController(final AppAuthService appAuthService,
                                         final PluginService pluginService,
                                         final SelectorService selectorService,
                                         final RuleService ruleService,
                                         final MetaDataService metaDataService,
                                         final ShenyuDictService shenyuDictService,
                                         final ProxySelectorService proxySelectorService,
                                         final DiscoveryService discoveryService,
                                         final DiscoveryUpstreamService discoveryUpstreamService) {
        this.appAuthService = appAuthService;
        this.pluginService = pluginService;
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.metaDataService = metaDataService;
        this.shenyuDictService = shenyuDictService;
        this.proxySelectorService = proxySelectorService;
        this.discoveryService = discoveryService;
        this.discoveryUpstreamService = discoveryUpstreamService;
    }

    /**
     * Export all configs.
     *
     * @param response response
     * @return the shenyu result
     */
    @GetMapping("/export")
    public ResponseEntity<byte[]> exportConfigs(final HttpServletResponse response) {
        List<ZipUtil.ZipItem> zipItemList = Lists.newArrayList();
        List<AppAuthVO> authDataList = appAuthService.listAllVO();
        if (CollectionUtils.isNotEmpty(authDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.AUTH_JSON, JsonUtils.toJson(authDataList)));
        }

        List<MetaDataVO> metaDataList = metaDataService.listAllVO();
        if (CollectionUtils.isNotEmpty(metaDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.META_JSON, JsonUtils.toJson(metaDataList)));
        }

        List<PluginVO> pluginDataList = pluginService.listAllVO();
        if (CollectionUtils.isNotEmpty(pluginDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.PLUGIN_JSON, JsonUtils.toJson(pluginDataList)));
        }

        List<SelectorVO> selectorDataList = selectorService.listAllVO();
        if (CollectionUtils.isNotEmpty(selectorDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.SELECTOR_JSON, JsonUtils.toJson(selectorDataList)));
        }

        List<RuleVO> ruleDataList = ruleService.listAllVO();
        if (CollectionUtils.isNotEmpty(ruleDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.RULE_JSON, JsonUtils.toJson(ruleDataList)));
        }

        List<ShenyuDictVO> dictDataList = shenyuDictService.listAll();
        if (CollectionUtils.isNotEmpty(dictDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.DICT_JSON, JsonUtils.toJson(dictDataList)));
        }

        List<ProxySelectorData> proxySelectorDataList = proxySelectorService.listAll();
        if (CollectionUtils.isNotEmpty(proxySelectorDataList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.PROXY_SELECTOR_JSON, JsonUtils.toJson(proxySelectorDataList)));
        }

        List<DiscoveryVO> discoveryList = discoveryService.listAllVO();
        if (CollectionUtils.isNotEmpty(discoveryList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.DISCOVERY_JSON, JsonUtils.toJson(discoveryList)));
        }

        List<DiscoveryUpstreamVO> discoveryUpstreamList = discoveryUpstreamService.listAllVO();
        if (CollectionUtils.isNotEmpty(discoveryUpstreamList)) {
            zipItemList.add(new ZipUtil.ZipItem(ExportImportConstants.DISCOVERY_UPSTREAM_JSON, JsonUtils.toJson(discoveryUpstreamList)));
        }

        HttpHeaders headers = new HttpHeaders();
        String fileName = generateFileName();
        response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
        headers.add("Content-Disposition", "attachment;filename=" + fileName);
        return new ResponseEntity<>(ZipUtil.zip(zipItemList), headers, HttpStatus.OK);
    }

    /**
     * generate export file name.
     *
     * @return fileName
     */
    private String generateFileName() {
        return ExportImportConstants.EXPORT_CONFIG_FILE_NAME + DateFormatUtils.format(new Date(), ExportImportConstants.EXPORT_CONFIG_FILE_NAME_DATE_FORMAT)
                + ExportImportConstants.EXPORT_CONFIG_FILE_NAME_EXT;
    }

    /**
     * Import configs.
     *
     * @param file config file
     * @return shenyu admin result
     */
    @PostMapping("/import")
    public ShenyuAdminResult importConfigs(final MultipartFile file) {
        if (Objects.isNull(file)) {
            return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
        }
        try {
            Map<String, Object> result = Maps.newHashMap();
            ZipUtil.UnZipResult unZipResult = ZipUtil.unzip(file.getBytes());
            List<ZipUtil.ZipItem> zipItemList = unZipResult.getZipItemList();
            if (CollectionUtils.isEmpty(zipItemList)) {
                LOG.info("import file is empty");
                return ShenyuAdminResult.success();
            }

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
        } catch (IOException e) {
            LOG.error("parsing data failed", e);
            return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
        }

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
            ConfigImportResult configImportResult = pluginService.importData(pluginList);
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
