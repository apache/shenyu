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
import org.apache.shenyu.admin.model.dto.SelectorExportDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.service.ConfigsService;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.service.SelectiveConfigsExportService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shiro.authz.UnauthorizedException;
import org.apache.shiro.spring.security.interceptor.AuthorizationAttributeSourceAdvisor;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.aop.framework.ProxyFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
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
    private SelectiveConfigsExportService selectiveConfigsExportService;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders
                .standaloneSetup(configsExportImportController)
                .setControllerAdvice(new ExceptionHandlers(null))
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
    public void testExportSelectedSelectors() throws Exception {
        byte[] archive = ZipUtil.zip(List.of(new ZipUtil.ZipItem("selector.json", "[]")));
        when(selectiveConfigsExportService.export(any())).thenReturn(archive);
        MockHttpServletResponse response = mockMvc.perform(post("/configs/exportBySelectors")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"namespace\":\"test\",\"selectorIds\":[\"orders\"]}"))
                .andExpect(status().isOk()).andReturn().getResponse();
        assertThat(response.getContentAsByteArray()).isEqualTo(archive);
        assertThat(response.getHeader("Content-Disposition")).endsWith(".zip");
    }

    @Test
    public void testExportRejectsEmptySelection() throws Exception {
        mockMvc.perform(post("/configs/exportBySelectors").contentType(MediaType.APPLICATION_JSON)
                        .content("{\"namespace\":\"test\",\"selectorIds\":[]}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", containsString("selectorIds")));
        verify(selectiveConfigsExportService, never()).export(any());
    }

    @Test
    public void testExportRequiresExistingExportPermission() {
        Subject subject = mock(Subject.class);
        doThrow(new UnauthorizedException("denied")).when(subject).checkPermission("system:manager:exportConfig");
        ProxyFactory factory = new ProxyFactory(configsExportImportController);
        factory.addAdvisor(new AuthorizationAttributeSourceAdvisor());
        ConfigsExportImportController secured = (ConfigsExportImportController) factory.getProxy();
        ThreadContext.bind(subject);
        try {
            assertThrows(UnauthorizedException.class, () -> secured.exportConfigsBySelectors(new SelectorExportDTO(), new MockHttpServletResponse()));
            verify(selectiveConfigsExportService, never()).export(any());
        } finally {
            ThreadContext.unbindSubject();
        }
    }

    @Test
    public void testImportConfigs() throws Exception {

        // mock import data
        List<ZipUtil.ZipItem> zipItemList = Lists.newArrayList();

        when(this.configsService.configsImport(anyString(), any())).thenReturn(
                ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS));
        when(this.namespaceService.findByNamespaceId("test")).thenReturn(new NamespaceVO());

        // mock file
        MockMultipartFile file = new MockMultipartFile("file", "test.zip", MediaType.TEXT_PLAIN_VALUE, ZipUtil.zip(zipItemList));
        // Run the test
        final MockHttpServletResponse response = mockMvc.perform(multipart("/configs/import")
                        .file(file)
                        .param("namespace", "test")
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.SUCCESS)))
                .andExpect(status().isOk())
                .andReturn().getResponse();

        // Verify the results
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
    }

    @Test
    public void testImportConfigsWithUnknownNamespace() throws Exception {
        MockMultipartFile file = new MockMultipartFile("file", "test.zip", MediaType.TEXT_PLAIN_VALUE, new byte[]{1});
        when(this.namespaceService.findByNamespaceId("ghost-ns")).thenReturn(null);

        mockMvc.perform(multipart("/configs/import")
                        .file(file)
                        .param("namespace", "ghost-ns")
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.message", is("namespace is not exist")))
                .andExpect(status().isOk());

        verify(this.configsService, never()).configsImport(anyString(), any());
    }

}
