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
import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.AppAuthVO;
import org.apache.shenyu.admin.model.vo.MetaDataVO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shenyu.common.constant.ExportImportConstants;
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
     * The Metadata service.
     */
    private final MetaDataService metaDataService;


    public ConfigsExportImportController(final AppAuthService appAuthService, final PluginService pluginService, final MetaDataService metaDataService) {
        this.appAuthService = appAuthService;
        this.pluginService = pluginService;
        this.metaDataService = metaDataService;
    }


    /**
     * Export all configs.
     *
     * @return the shenyu result
     */
    @GetMapping("/export")
    public ResponseEntity<byte[]> exportConfigs(HttpServletResponse response) {
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
     * @param file config file
     * @return shenyu admin result
     */
    @PostMapping("/import")
    public ShenyuAdminResult importConfigs(MultipartFile file) {
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
                    case ExportImportConstants.PLUGIN_JSON:
                        String pluginJson = zipItem.getItemData();
                        if (StringUtils.isNotEmpty(pluginJson)) {
                            List<PluginDTO> pluginList = GsonUtils.getInstance().fromList(pluginJson, PluginDTO.class);
                            ConfigImportResult configImportResult = pluginService.importData(pluginList);
                            result.put(ExportImportConstants.PLUGIN_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
                            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                                result.put(ExportImportConstants.PLUGIN_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
                            }
                        }
                        break;
                    case ExportImportConstants.AUTH_JSON:
                        String authJson = zipItem.getItemData();
                        if (StringUtils.isNotEmpty(authJson)) {
                            List<AppAuthDTO> authDataList = GsonUtils.getInstance().fromList(authJson, AppAuthDTO.class);
                            ConfigImportResult configImportResult = appAuthService.importData(authDataList);
                            result.put(ExportImportConstants.AUTH_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
                            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                                result.put(ExportImportConstants.AUTH_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
                            }
                        }
                        break;
                    case ExportImportConstants.META_JSON:
                        String metaJson = zipItem.getItemData();
                        if (StringUtils.isNotEmpty(metaJson)) {
                            List<MetaDataDTO> metaDataList = GsonUtils.getInstance().fromList(metaJson, MetaDataDTO.class);
                            ConfigImportResult configImportResult = metaDataService.importData(metaDataList);
                            result.put(ExportImportConstants.META_IMPORT_SUCCESS_COUNT, configImportResult.getSuccessCount());
                            if (StringUtils.isNotEmpty(configImportResult.getFailMessage())) {
                                result.put(ExportImportConstants.META_IMPORT_FAIL_MESSAGE, configImportResult.getFailMessage());
                            }
                        }
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
}
