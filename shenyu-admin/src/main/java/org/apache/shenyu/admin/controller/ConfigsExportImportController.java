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
import org.apache.shenyu.admin.model.enums.PreviewTokenCheckResult;
import org.apache.shenyu.admin.model.result.ConfigsImportWarning;
import org.apache.shenyu.admin.model.dto.SelectorExportDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.ConfigsService;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.service.SelectiveConfigsExportService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.service.configs.plan.ConfigsImportErrors;
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
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * this is configs controller.
 */
@RestApi("/configs")
public class ConfigsExportImportController {

    private static final Logger LOG = LoggerFactory.getLogger(ConfigsExportImportController.class);

    private static final String PREVIEW_TOKEN_WARNING = "PREVIEW_TOKEN_WARNING";

    private static final String PREVIEW_TOKEN_MISSING = "PREVIEW_TOKEN_MISSING";

    private static final String PREVIEW_TOKEN_STALE = "PREVIEW_TOKEN_STALE";

    private static final String PREVIEW_TOKEN_UNVERIFIABLE = "PREVIEW_TOKEN_UNVERIFIABLE";

    /**
     * The config service.
     */
    private final ConfigsService configsService;

    private final SyncDataService syncDataService;

    private final NamespaceService namespaceService;

    private final ConfigsImportPreviewService previewService;

    private final SelectiveConfigsExportService selectiveConfigsExportService;

    public ConfigsExportImportController(final ConfigsService configsService,
                                         final SyncDataService syncDataService,
                                         final NamespaceService namespaceService,
                                         final SelectiveConfigsExportService selectiveConfigsExportService) {
                                         final NamespaceService namespaceService,
                                         final ConfigsImportPreviewService previewService) {
        this.configsService = configsService;
        this.syncDataService = syncDataService;
        this.namespaceService = namespaceService;
        this.previewService = previewService;
        this.selectiveConfigsExportService = selectiveConfigsExportService;
    }

    /**
     * Export selected static HTTP divide routes in the existing namespace import format.
     *
     * @param request source namespace and selector IDs
     * @param response response
     * @return ZIP archive
     */
    @PostMapping("/exportBySelectors")
    @RequiresPermissions("system:manager:exportConfig")
    public ResponseEntity<byte[]> exportConfigsBySelectors(@Valid @RequestBody final SelectorExportDTO request, final HttpServletResponse response) {
        byte[] archive = selectiveConfigsExportService.export(request);
        HttpHeaders headers = new HttpHeaders();
        response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
        headers.add("Content-Disposition", "attachment;filename=" + generateFileName());
        return new ResponseEntity<>(archive, headers, HttpStatus.OK);
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
            return ConfigsImportErrors.invalidArchive();
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
     * @param previewToken optional token from a matching preview
     * @return shenyu admin result
     */
    @PostMapping("/import")
    @RequiresPermissions("system:manager:importConfig")
    public ShenyuAdminResult importConfigs(final String namespace, final MultipartFile file,
                                           @RequestParam(required = false) final String previewToken) {
        if (StringUtils.isBlank(namespace) || Objects.isNull(file)) {
            return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
        }
        if (Objects.isNull(namespaceService.findByNamespaceId(namespace))) {
            return ShenyuAdminResult.error("namespace is not exist");
        }
        try {
            final byte[] source = file.getBytes();
            final ConfigsImportWarning previewWarning = previewWarning(namespace, source, previewToken);
            ShenyuAdminResult importResult = configsService.configsImport(namespace, source);
            addPreviewWarning(importResult, previewWarning);
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

    private ConfigsImportWarning previewWarning(final String namespace, final byte[] source, final String previewToken) {
        if (StringUtils.isBlank(previewToken)) {
            LOG.warn("configs import proceeded without a preview token, namespace={}", namespace);
            return new ConfigsImportWarning(PREVIEW_TOKEN_MISSING,
                    "import proceeded without a preview token; drift was not checked");
        }
        try {
            PreviewTokenCheckResult result = previewService.matchesToken(namespace, source, previewToken);
            if (result == PreviewTokenCheckResult.VALID) {
                return null;
            }
            if (result == PreviewTokenCheckResult.STALE) {
                LOG.warn("configs import proceeded with a stale preview token, namespace={}", namespace);
                return new ConfigsImportWarning(PREVIEW_TOKEN_STALE,
                        "import proceeded because the archive or target state changed since preview; drift was not verified");
            }
            LOG.warn("configs import proceeded because the preview token could not be verified, namespace={}", namespace);
            return new ConfigsImportWarning(PREVIEW_TOKEN_UNVERIFIABLE,
                    "import proceeded because the preview token could not be verified; drift was not checked");
        } catch (RuntimeException e) {
            LOG.warn("configs import proceeded because the preview token could not be verified, namespace={}", namespace, e);
            return new ConfigsImportWarning(PREVIEW_TOKEN_UNVERIFIABLE,
                    "import proceeded because the preview token could not be verified; drift was not checked");
        }
    }

    @SuppressWarnings("unchecked")
    private void addPreviewWarning(final ShenyuAdminResult result, final ConfigsImportWarning warning) {
        if (Objects.isNull(warning)) {
            return;
        }
        if (result.getData() instanceof Map<?, ?>) {
            ((Map<String, Object>) result.getData()).put(PREVIEW_TOKEN_WARNING, warning);
            return;
        }
        if (Objects.isNull(result.getData()) && Objects.equals(CommonErrorCode.SUCCESSFUL, result.getCode())) {
            Map<String, Object> data = new HashMap<>(1);
            data.put(PREVIEW_TOKEN_WARNING, warning);
            result.setData(data);
        }
    }

}
