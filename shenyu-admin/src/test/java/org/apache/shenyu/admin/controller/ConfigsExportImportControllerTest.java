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

import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.model.result.ConfigsImportPreview;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.service.ConfigsService;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.service.configs.plan.ConfigsImportPreviewBusyException;
import org.apache.shenyu.admin.service.configs.plan.ConfigsImportPreviewService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for ConfigsExportImportController.
 */
@ExtendWith(MockitoExtension.class)
public class ConfigsExportImportControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private ConfigsExportImportController configsExportImportController;

    /**
     * The configs service.
     */
    @Mock
    private ConfigsService configsService;

    /**
     * The configs service.
     */
    @Mock
    private SyncDataService syncDataService;

    @Mock
    private NamespaceService namespaceService;

    @Mock
    private ConfigsImportPreviewService previewService;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders
                .standaloneSetup(configsExportImportController)
                .setControllerAdvice(new ExceptionHandlers(null))
                .setControllerAdvice(configsService)
                .setControllerAdvice(syncDataService)
                .build();
    }

    @Test
    public void testExportConfigs() throws Exception {

        when(this.configsService.configsExport()).thenReturn(
                ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS));

        // Run the test
        final MockHttpServletResponse response = mockMvc.perform(get("/configs/export")
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn().getResponse();

        // Verify the results
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
    }

    @Test
    public void testImportConfigs() throws Exception {

        // mock import data
        List<ZipUtil.ZipItem> zipItemList = Lists.newArrayList();

        when(this.configsService.configsImport(anyString(), any())).thenReturn(
                ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS));
        when(this.namespaceService.findByNamespaceId("test")).thenReturn(new NamespaceVO());
        when(this.previewService.matchesToken(anyString(), any(), anyString())).thenReturn(true);

        // mock file
        MockMultipartFile file = new MockMultipartFile("file", "test.zip", MediaType.TEXT_PLAIN_VALUE, ZipUtil.zip(zipItemList));
        // Run the test
        final MockHttpServletResponse response = mockMvc.perform(multipart("/configs/import")
                        .file(file)
                        .param("namespace", "test")
                        .param("previewToken", "current")
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.SUCCESS)))
                .andExpect(status().isOk())
                .andReturn().getResponse();

        // Verify the results
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
    }

    @Test
    public void testImportRequiresPreviewToken() throws Exception {
        MockMultipartFile file = new MockMultipartFile("file", "test.zip", MediaType.TEXT_PLAIN_VALUE,
                ZipUtil.zip(Lists.newArrayList()));
        when(this.namespaceService.findByNamespaceId("test")).thenReturn(new NamespaceVO());

        mockMvc.perform(multipart("/configs/import")
                        .file(file)
                        .param("namespace", "test")
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(HttpStatus.BAD_REQUEST.value()))
                .andExpect(jsonPath("$.message", is("previewToken is required; preview the archive before importing")));

        verify(this.configsService, never()).configsImport(anyString(), any());
    }

    @Test
    public void testImportConfigsWithUnknownNamespace() throws Exception {
        MockMultipartFile file = new MockMultipartFile("file", "test.zip", MediaType.TEXT_PLAIN_VALUE, new byte[]{1});
        when(this.namespaceService.findByNamespaceId("ghost-ns")).thenReturn(null);

        mockMvc.perform(multipart("/configs/import")
                        .file(file)
                        .param("namespace", "ghost-ns")
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.code").value(HttpStatus.BAD_REQUEST.value()))
                .andExpect(jsonPath("$.message", is("namespace is not exist")))
                .andExpect(status().isOk());

        verify(this.configsService, never()).configsImport(anyString(), any());
    }

    @Test
    public void testPreviewImportConfigs() throws Exception {
        MockMultipartFile file = new MockMultipartFile("file", "test.zip", MediaType.TEXT_PLAIN_VALUE,
                ZipUtil.zip(Lists.newArrayList()));
        ConfigsImportPreview preview = new ConfigsImportPreview("2026-11-04T00:00:00Z", "sha256:a", "sha256:b",
                "sha256:c", "test", true, false, new ConfigsImportPreview.Summary(1, 0, 0, 0),
                List.of(), List.of("OBSERVATION_ONLY"));
        when(this.namespaceService.findByNamespaceId("test")).thenReturn(new NamespaceVO());
        when(this.previewService.preview(anyString(), any())).thenReturn(preview);

        mockMvc.perform(multipart("/configs/import/preview")
                        .file(file)
                        .param("namespace", "test")
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(CommonErrorCode.SUCCESSFUL))
                .andExpect(jsonPath("$.data.targetNamespace").value("test"))
                .andExpect(jsonPath("$.data.fullyValidated").value(true));
    }

    @Test
    public void testPreviewUnknownNamespace() throws Exception {
        MockMultipartFile file = new MockMultipartFile("file", "test.zip", MediaType.TEXT_PLAIN_VALUE,
                ZipUtil.zip(Lists.newArrayList()));
        when(this.namespaceService.findByNamespaceId("ghost-ns")).thenReturn(null);

        mockMvc.perform(multipart("/configs/import/preview")
                        .file(file)
                        .param("namespace", "ghost-ns")
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(HttpStatus.BAD_REQUEST.value()))
                .andExpect(jsonPath("$.message", is("namespace is not exist")));
    }

    @Test
    public void testPreviewReportsBusy() throws Exception {
        MockMultipartFile file = new MockMultipartFile("file", "test.zip", MediaType.TEXT_PLAIN_VALUE,
                ZipUtil.zip(Lists.newArrayList()));
        when(this.namespaceService.findByNamespaceId("test")).thenReturn(new NamespaceVO());
        when(this.previewService.preview(anyString(), any())).thenThrow(new ConfigsImportPreviewBusyException());

        mockMvc.perform(multipart("/configs/import/preview")
                        .file(file)
                        .param("namespace", "test")
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(HttpStatus.TOO_MANY_REQUESTS.value()));
    }

    @Test
    public void testImportAcceptsCurrentPreviewToken() throws Exception {
        MockMultipartFile file = new MockMultipartFile("file", "test.zip", MediaType.TEXT_PLAIN_VALUE,
                ZipUtil.zip(Lists.newArrayList()));
        when(this.namespaceService.findByNamespaceId("test")).thenReturn(new NamespaceVO());
        when(this.previewService.matchesToken(anyString(), any(), anyString())).thenReturn(true);
        when(this.configsService.configsImport(anyString(), any())).thenReturn(
                ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS));

        mockMvc.perform(multipart("/configs/import")
                        .file(file)
                        .param("namespace", "test")
                        .param("previewToken", "current")
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.SUCCESS)));

        verify(this.configsService).configsImport(anyString(), any());
    }

    @Test
    public void testImportRejectsStalePreviewToken() throws Exception {
        MockMultipartFile file = new MockMultipartFile("file", "test.zip", MediaType.TEXT_PLAIN_VALUE,
                ZipUtil.zip(Lists.newArrayList()));
        when(this.namespaceService.findByNamespaceId("test")).thenReturn(new NamespaceVO());
        when(this.previewService.matchesToken(anyString(), any(), anyString())).thenReturn(false);

        mockMvc.perform(multipart("/configs/import")
                        .file(file)
                        .param("namespace", "test")
                        .param("previewToken", "stale")
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(HttpStatus.CONFLICT.value()))
                .andExpect(jsonPath("$.message", is("Preview is stale; preview again before importing")));

        verify(this.configsService, never()).configsImport(anyString(), any());
    }

}
