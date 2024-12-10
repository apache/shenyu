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

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.ConfigsService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.ExportImportConstants;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Date;
import java.util.Objects;

/**
 * this is configs controller.
 */
@RestApi("/configs")
public class ConfigsExportImportController {

    private static final Logger LOG = LoggerFactory.getLogger(ConfigsExportImportController.class);

    /**
     * The config service.
     */
    private final ConfigsService configsService;

    private final SyncDataService syncDataService;

    public ConfigsExportImportController(final ConfigsService configsService,
                                         final SyncDataService syncDataService) {
        this.configsService = configsService;
        this.syncDataService = syncDataService;
    }

    /**
     * Export all configs.
     *
     * @param response response
     * @return the shenyu result
     */
    @GetMapping("/export")
    @RequiresPermissions("system:manager:exportConfig")
    public ResponseEntity<byte[]> exportConfigs(final HttpServletResponse response) {
        ShenyuAdminResult result = configsService.configsExport();
        if (!Objects.equals(CommonErrorCode.SUCCESSFUL, result.getCode())) {
            throw new ShenyuException(result.getMessage());
        }
        HttpHeaders headers = new HttpHeaders();
        String fileName = generateFileName();
        response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
        headers.add("Content-Disposition", "attachment;filename=" + fileName);
        return new ResponseEntity<>((byte[]) result.getData(), headers, HttpStatus.OK);
    }

    /**
     * Export all configs.
     *
     * @param namespace namespaceId
     * @param response response
     * @return the shenyu result
     */
    @GetMapping("/exportByNamespace")
    @RequiresPermissions("system:manager:exportConfig")
    public ResponseEntity<byte[]> exportConfigsByNamespace(final String namespace, final HttpServletResponse response) {
        ShenyuAdminResult result = configsService.configsExport(namespace);
        if (!Objects.equals(CommonErrorCode.SUCCESSFUL, result.getCode())) {
            throw new ShenyuException(result.getMessage());
        }
        HttpHeaders headers = new HttpHeaders();
        String fileName = generateFileName(namespace);
        response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
        headers.add("Content-Disposition", "attachment;filename=" + fileName);
        return new ResponseEntity<>((byte[]) result.getData(), headers, HttpStatus.OK);
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

    private String generateFileName(final String namespace) {
        return ExportImportConstants.EXPORT_CONFIG_FILE_NAME + namespace + "_" + DateFormatUtils.format(new Date(), ExportImportConstants.EXPORT_CONFIG_FILE_NAME_DATE_FORMAT)
                + ExportImportConstants.EXPORT_CONFIG_FILE_NAME_EXT;
    }


    /**
     * Import configs.
     *
     * @param namespace namespace
     * @param file config file
     * @return shenyu admin result
     */
    @PostMapping("/import")
    @RequiresPermissions("system:manager:importConfig")
    public ShenyuAdminResult importConfigs(final String namespace, final MultipartFile file) {
        if (StringUtils.isBlank(namespace) || Objects.isNull(file)) {
            return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
        }
        try {
            ShenyuAdminResult importResult = configsService.configsImport(namespace, file.getBytes());
            if (Objects.equals(CommonErrorCode.SUCCESSFUL, importResult.getCode())) {
                // sync data
                syncDataService.syncAll(DataEventTypeEnum.REFRESH);
            }
            return importResult;
        } catch (IOException e) {
            LOG.error("parsing data failed", e);
            return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
        }
    }

}
