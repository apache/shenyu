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

package org.apache.shenyu.admin.service.manager.impl;

import org.apache.shenyu.admin.mapper.ApiMapper;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.bean.DocParameter;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.service.manager.RegisterApiDocService;
import org.apache.shenyu.common.utils.JsonUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * test cases for DocManagerImpl.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DocManagerImplTest {
    @InjectMocks
    private DocManagerImpl docManager;

    @Mock
    private RegisterApiDocService registerApiDocService;

    @Mock
    private ApiMapper apiMapper;

    @Test
    public void testAddDocInfo() {
        UpstreamInstance instance = new UpstreamInstance();
        instance.setContextPath("/testClusterName");
        AtomicBoolean atomicBoolean = new AtomicBoolean(false);
        docManager.addDocInfo(instance, SwaggerDocParserTest.DOC_INFO_JSON, "", docInfo -> {
            Assertions.assertEquals(docInfo.getTitle(), "shenyu-examples-http-swagger3 API");
            Assertions.assertEquals(docInfo.getClusterName(), "testClusterName");
            atomicBoolean.set(true);
        });

        Assertions.assertTrue(atomicBoolean.get());
        verify(registerApiDocService, atLeastOnce()).registerApiDocument(any());
        verifyNoInteractions(apiMapper);
    }

    @Test
    public void testListAllReadsPersistedDocuments() {
        ApiDO orders = buildApi("orders-id", "/orders", "orders", "/orders/find");
        ApiDO payments = buildApi("payments-id", "/payments", "payments", "/payments/find");
        when(apiMapper.selectAllWithDocument()).thenReturn(List.of(payments, orders));

        List<DocInfo> projects = new ArrayList<>(docManager.listAll());

        Assertions.assertEquals(2, projects.size());
        Assertions.assertEquals("/orders", projects.get(0).getTitle());
        Assertions.assertEquals("/orders", projects.get(0).getContextPath());
        DocItem menuItem = projects.get(0).getDocModuleList().get(0).getDocItems().get(0);
        Assertions.assertEquals("orders-id", menuItem.getId());
        Assertions.assertEquals("/orders/find", menuItem.getName());
        Assertions.assertEquals("/payments", projects.get(1).getTitle());
        verifyNoInteractions(registerApiDocService);
    }

    @Test
    public void testGetDocItemReturnsStoredContentAndDatabaseId() {
        ApiDO api = buildApi("api-id", "/orders", "orders", "/orders/find");
        when(apiMapper.selectByPrimaryKey("api-id")).thenReturn(api);

        DocItem docItem = docManager.getDocItem("api-id");

        Assertions.assertEquals("api-id", docItem.getId());
        Assertions.assertEquals("/orders/find", docItem.getName());
        Assertions.assertEquals("Find an order", docItem.getSummary());
        Assertions.assertEquals(List.of("get"), new ArrayList<>(docItem.getHttpMethodList()));
        Assertions.assertEquals("orderId", docItem.getRequestParameters().get(0).getName());
        verifyNoInteractions(registerApiDocService);
    }

    @Test
    public void testGetDocItemMissingOrBlank() {
        Assertions.assertNull(docManager.getDocItem("missing"));
        Assertions.assertNull(docManager.getDocItem(null));
        Assertions.assertNull(docManager.getDocItem(" "));
        verify(apiMapper).selectByPrimaryKey("missing");
    }

    @Test
    public void testInvalidDocumentsDoNotBreakMenusOrDetail() {
        List<ApiDO> apis = new ArrayList<>();
        for (String document : List.of("", " ", "not-json", "null", "{}", "{\"operationId\":\"/orders/find\"}")) {
            ApiDO api = new ApiDO();
            api.setId("invalid-" + apis.size());
            api.setDocument(document);
            apis.add(api);
            when(apiMapper.selectByPrimaryKey(api.getId())).thenReturn(api);
            Assertions.assertNull(docManager.getDocItem(api.getId()));
        }
        ApiDO valid = buildApi("valid-id", "/orders", "orders", "/orders/find");
        apis.add(valid);
        when(apiMapper.selectAllWithDocument()).thenReturn(apis);

        List<DocInfo> projects = new ArrayList<>(docManager.listAll());

        Assertions.assertEquals(1, projects.size());
        Assertions.assertEquals("valid-id", projects.get(0).getDocModuleList().get(0).getDocItems().get(0).getId());
    }

    @Test
    public void testListAllPreservesModuleAndApiOrdering() {
        ApiDO first = buildApi("first-id", "/orders", "first", "/orders/b");
        ApiDO second = buildApi("second-id", "/orders", "second", "/orders/a");
        ApiDO third = buildApi("third-id", "/orders", "first", "/orders/a");
        DocItem secondItem = JsonUtils.jsonToObject(second.getDocument(), DocItem.class);
        secondItem.setModuleOrder(1);
        second.setDocument(JsonUtils.toJson(secondItem));
        when(apiMapper.selectAllWithDocument()).thenReturn(List.of(second, first, third));

        DocInfo project = docManager.listAll().iterator().next();

        Assertions.assertEquals("first", project.getDocModuleList().get(0).getModule());
        Assertions.assertEquals("second", project.getDocModuleList().get(1).getModule());
        Assertions.assertEquals("third-id", project.getDocModuleList().get(0).getDocItems().get(0).getId());
        Assertions.assertEquals("first-id", project.getDocModuleList().get(0).getDocItems().get(1).getId());
    }

    @Test
    public void testListAllHandlesMissingModuleAndContextPath() {
        ApiDO api = buildApi("api-id", null, null, "/orders/find");
        when(apiMapper.selectAllWithDocument()).thenReturn(List.of(api));

        DocInfo project = docManager.listAll().iterator().next();

        Assertions.assertEquals("/", project.getContextPath());
        Assertions.assertEquals("default", project.getDocModuleList().get(0).getModule());
    }

    private ApiDO buildApi(final String id, final String contextPath, final String module, final String path) {
        DocItem docItem = new DocItem();
        docItem.setId("stale-document-id");
        docItem.setName(path);
        docItem.setModule(module);
        docItem.setSummary("Find an order");
        docItem.setHttpMethodList(Set.of("get"));
        DocParameter parameter = new DocParameter();
        parameter.setName("orderId");
        parameter.setType("string");
        docItem.setRequestParameters(List.of(parameter));
        ApiDO api = new ApiDO();
        api.setId(id);
        api.setContextPath(contextPath);
        api.setApiPath(path);
        api.setDocument(JsonUtils.toJson(docItem));
        return api;
    }
}
