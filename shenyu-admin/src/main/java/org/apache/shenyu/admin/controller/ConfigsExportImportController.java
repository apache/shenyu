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
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.service.configs.plan.ConfigsImportPreviewBusyException;
import org.apache.shenyu.admin.service.configs.plan.ConfigsImportPreviewService;
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
import org.springframework.web.bind.annotation.RequestParam;
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

    private final NamespaceService namespaceService;

    private final ConfigsImportPreviewService previewService;

    public ConfigsExportImportController(final ConfigsService configsService,
                                         final SyncDataService syncDataService,
                                         final NamespaceService namespaceService,
                                         final ConfigsImportPreviewService previewService) {
        this.configsService = configsService;
        this.syncDataService = syncDataService;
        this.namespaceService = namespaceService;
        this.previewService = previewService;
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
     * Preview an import archive without writing configuration or publishing events.
     *
     * @param namespace namespace
     * @param file config file
     * @return shenyu admin result
     */
    @PostMapping("/import/preview")
    @RequiresPermissions("system:manager:importConfig")
    public ShenyuAdminResult previewImportConfigs(final String namespace, final MultipartFile file) {
        if (StringUtils.isBlank(namespace) || Objects.isNull(file)) {
            return ShenyuAdminResult.error(HttpStatus.BAD_REQUEST.value(), ShenyuResultMessage.PARAMETER_ERROR);
        }
        if (Objects.isNull(namespaceService.findByNamespaceId(namespace))) {
            return ShenyuAdminResult.error(HttpStatus.BAD_REQUEST.value(), "namespace is not exist");
        }
        try {
            return ShenyuAdminResult.success(previewService.preview(namespace, file.getBytes()));
        } catch (ConfigsImportPreviewBusyException e) {
            return ShenyuAdminResult.error(HttpStatus.TOO_MANY_REQUESTS.value(), e.getMessage());
        } catch (IllegalArgumentException e) {
            return ShenyuAdminResult.error(HttpStatus.BAD_REQUEST.value(), "Import failed: " + e.getMessage());
        } catch (IOException e) {
            LOG.error("parsing data failed", e);
            return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
        } catch (RuntimeException e) {
            LOG.error("Failed to preview configs import", e);
            return ShenyuAdminResult.error("Preview failed");
        }
    }

    /**
     * Import configs.
     *
     * @param namespace namespace
     * @param file config file
     * @param previewToken token from a matching preview
     * @return shenyu admin result
     */
    @PostMapping("/import")
    @RequiresPermissions("system:manager:importConfig")
    public ShenyuAdminResult importConfigs(final String namespace, final MultipartFile file,
                                           @RequestParam(required = false) final String previewToken) {
        if (StringUtils.isBlank(namespace) || Objects.isNull(file)) {
            return ShenyuAdminResult.error(HttpStatus.BAD_REQUEST.value(), ShenyuResultMessage.PARAMETER_ERROR);
        }
        if (Objects.isNull(namespaceService.findByNamespaceId(namespace))) {
            return ShenyuAdminResult.error(HttpStatus.BAD_REQUEST.value(), "namespace is not exist");
        }
        if (StringUtils.isBlank(previewToken)) {
            return ShenyuAdminResult.error(HttpStatus.BAD_REQUEST.value(),
                    "previewToken is required; preview the archive before importing");
        }
        try {
            byte[] source = file.getBytes();
            if (!previewService.matchesToken(namespace, source, previewToken)) {
                return ShenyuAdminResult.error(HttpStatus.CONFLICT.value(), "Preview is stale; preview again before importing");
            }
            ShenyuAdminResult importResult = configsService.configsImport(namespace, source);
            if (Objects.equals(CommonErrorCode.SUCCESSFUL, importResult.getCode())) {
                // sync data only for the target namespace to avoid cross-namespace refresh events
                syncDataService.syncAllByNamespaceId(DataEventTypeEnum.REFRESH, namespace);
            }
            return importResult;
        } catch (IOException e) {
            LOG.error("parsing data failed", e);
            return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
        }
    }

}
