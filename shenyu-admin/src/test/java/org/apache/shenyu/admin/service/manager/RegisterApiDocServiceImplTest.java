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

package org.apache.shenyu.admin.service.manager;

import org.apache.shenyu.admin.model.dto.ApiDTO;
import org.apache.shenyu.admin.model.dto.TagDTO;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.ApiService;
import org.apache.shenyu.admin.service.manager.impl.RegisterApiDocServiceImpl;
import org.apache.shenyu.admin.service.TagService;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.Mock;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** Tests for {@link RegisterApiDocServiceImpl}. */
@ExtendWith(MockitoExtension.class)
public final class RegisterApiDocServiceImplTest {
    @Mock
    private ApiService apiService;

    @Mock
    private TagService tagService;

    @InjectMocks
    private RegisterApiDocServiceImpl service;

    @Test
    public void testRegisterUsesExistingAndCreatesMissingTags() {
        ReflectionTestUtils.setField(service, "apiService", apiService);
        ReflectionTestUtils.setField(service, "tagService", tagService);
        ApiDocRegisterDTO dto = new ApiDocRegisterDTO();
        dto.setEventType(EventType.REGISTER);
        dto.setApiPath("/a");
        dto.setHttpMethod(1);
        dto.setRpcType("http");
        dto.setTags(Collections.singletonList("tag"));
        when(tagService.findByQuery("tag")).thenReturn(Collections.emptyList());
        service.registerApiDocument(dto);
        verify(apiService).deleteByApiPathHttpMethodRpcType("/a", 1, "http");
        verify(tagService).createRootTag(any(), any());
        verify(apiService).createOrUpdate(any());
    }

    @Test
    public void testRegisterWithExistingTagAndOffline() {
        ReflectionTestUtils.setField(service, "apiService", apiService);
        ReflectionTestUtils.setField(service, "tagService", tagService);
        ApiDocRegisterDTO dto = new ApiDocRegisterDTO();
        dto.setEventType(EventType.REGISTER);
        dto.setContextPath("/ctx");
        dto.setTags(Collections.singletonList("tag"));
        TagVO tag = new TagVO();
        tag.setId("tag-id");
        when(tagService.findByQuery("tag")).thenReturn(Collections.singletonList(tag));
        service.registerApiDocument(dto);
        verify(apiService).createOrUpdate(any());
        ApiDocRegisterDTO offline = new ApiDocRegisterDTO();
        offline.setEventType(EventType.OFFLINE);
        offline.setContextPath("/ctx");
        service.registerApiDocument(offline);
        verify(apiService).offlineByContextPath("/ctx");
    }

    @Test
    public void testRegisterUsesContextPathWhenTagsAreAbsent() {
        ReflectionTestUtils.setField(service, "apiService", apiService);
        ReflectionTestUtils.setField(service, "tagService", tagService);
        ApiDocRegisterDTO dto = new ApiDocRegisterDTO();
        dto.setEventType(EventType.REGISTER);
        dto.setContextPath("/ctx");
        when(tagService.findByQuery("/ctx")).thenReturn(Collections.emptyList());
        service.registerApiDocument(dto);
        verify(tagService).createRootTag(any(), any());
        verify(apiService).createOrUpdate(any());
    }

    @Test
    public void testRegisterIgnoresOtherEventTypes() {
        ReflectionTestUtils.setField(service, "apiService", apiService);
        ReflectionTestUtils.setField(service, "tagService", tagService);
        ApiDocRegisterDTO dto = new ApiDocRegisterDTO();
        dto.setEventType(EventType.UPDATED);
        service.registerApiDocument(dto);
        org.mockito.Mockito.verifyNoInteractions(apiService, tagService);
    }

    @Test
    public void testRegisterCombinesExistingAndNewTagsAndCopiesApiFields() {
        ApiDocRegisterDTO dto = new ApiDocRegisterDTO();
        dto.setEventType(EventType.REGISTER);
        dto.setApiPath("/ctx/api");
        dto.setContextPath("/ctx");
        dto.setHttpMethod(1);
        dto.setRpcType("http");
        dto.setDocument("document");
        dto.setApiSource(1);
        dto.setApiOwner("owner");
        dto.setApiDesc("description");
        dto.setExt("{}");
        dto.setVersion("v1");
        dto.setConsume("application/json");
        dto.setProduce("application/json");
        dto.setState(1);
        dto.setTags(List.of("existing", "new"));
        TagVO tag = new TagVO();
        tag.setId("existing-id");
        when(tagService.findByQuery("existing")).thenReturn(Collections.singletonList(tag));
        when(tagService.findByQuery("new")).thenReturn(Collections.emptyList());
        service.registerApiDocument(dto);
        ArgumentCaptor<TagDTO> tagCaptor = ArgumentCaptor.forClass(TagDTO.class);
        verify(tagService).createRootTag(tagCaptor.capture(), org.mockito.ArgumentMatchers.isNull());
        ArgumentCaptor<ApiDTO> apiCaptor = ArgumentCaptor.forClass(ApiDTO.class);
        verify(apiService).createOrUpdate(apiCaptor.capture());
        ApiDTO api = apiCaptor.getValue();
        assertNotNull(tagCaptor.getValue().getId());
        assertEquals(List.of("existing-id", tagCaptor.getValue().getId()), api.getTagIds());
        assertEquals("new", tagCaptor.getValue().getTagName());
        assertEquals("/ctx/api", api.getApiPath());
        assertEquals("/ctx", api.getContextPath());
        assertEquals(1, api.getHttpMethod());
        assertEquals("http", api.getRpcType());
        assertEquals("document", api.getDocument());
        assertEquals(1, api.getApiSource());
        assertEquals("owner", api.getApiOwner());
        assertEquals("description", api.getApiDesc());
        assertEquals("{}", api.getExt());
        assertEquals("v1", api.getVersion());
        assertEquals("application/json", api.getConsume());
        assertEquals("application/json", api.getProduce());
        assertEquals(1, api.getState());
    }
}
