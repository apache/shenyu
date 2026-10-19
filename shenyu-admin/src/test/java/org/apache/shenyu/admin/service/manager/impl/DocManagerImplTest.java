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

import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.service.manager.RegisterApiDocService;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.ApiSourceEnum;
import org.apache.shenyu.common.enums.ApiStateEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.util.DigestUtils;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

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

    @Test
    public void testAddDocInfo() {
        UpstreamInstance instance = new UpstreamInstance();
        instance.setContextPath("/testClusterName");
        instance.setIp("127.0.0.1");
        instance.setPort(8190);
        AtomicBoolean atomicBoolean = new AtomicBoolean(false);
        docManager.addDocInfo(instance, SwaggerDocParserTest.DOC_INFO_JSON, "", docInfo -> {
            Assertions.assertEquals(docInfo.getTitle(), "shenyu-examples-http-swagger3 API");
            Assertions.assertEquals(docInfo.getClusterName(), "testClusterName");
            Assertions.assertEquals(DigestUtils.md5DigestAsHex(SwaggerDocParserTest.DOC_INFO_JSON.getBytes(StandardCharsets.UTF_8)), docInfo.getDocMd5());
            atomicBoolean.set(true);
        });

        Assertions.assertTrue(atomicBoolean.get());

        ArgumentCaptor<ApiDocRegisterDTO> captor = ArgumentCaptor.forClass(ApiDocRegisterDTO.class);
        verify(registerApiDocService).registerApiDocument(captor.capture());
        ApiDocRegisterDTO document = captor.getValue();
        Assertions.assertEquals("/testClusterName", document.getContextPath());
        Assertions.assertEquals(ApiHttpMethodEnum.GET.getValue(), document.getHttpMethod());
        Assertions.assertEquals(ApiSourceEnum.SWAGGER.getValue(), document.getApiSource());
        Assertions.assertEquals(ApiStateEnum.UNPUBLISHED.getState(), document.getState());
        Assertions.assertEquals(RpcTypeEnum.HTTP.getName(), document.getRpcType());
        Assertions.assertEquals(EventType.REGISTER, document.getEventType());
        DocItem docItem = GsonUtils.getInstance().fromJson(document.getDocument(), DocItem.class);
        Assertions.assertEquals("Get path variable", docItem.getSummary());
        Assertions.assertEquals(docItem.getName(), document.getApiPath());
        ApiDocRegisterDTO.ApiExt ext = GsonUtils.getInstance().fromJson(document.getExt(), ApiDocRegisterDTO.ApiExt.class);
        Assertions.assertEquals("127.0.0.1", ext.getHost());
        Assertions.assertEquals(8190, ext.getPort());
        Assertions.assertEquals("testClusterName", ext.getServiceName());
    }

    @Test
    public void testUnchangedDocIsNotRegisteredAgain() {
        UpstreamInstance instance = new UpstreamInstance();
        instance.setContextPath("/testClusterName");
        String md5 = DigestUtils.md5DigestAsHex(SwaggerDocParserTest.DOC_INFO_JSON.getBytes(StandardCharsets.UTF_8));
        docManager.addDocInfo(instance, SwaggerDocParserTest.DOC_INFO_JSON, md5, docInfo -> Assertions.fail("Unchanged document should be skipped"));

        verifyNoInteractions(registerApiDocService);
    }
}
