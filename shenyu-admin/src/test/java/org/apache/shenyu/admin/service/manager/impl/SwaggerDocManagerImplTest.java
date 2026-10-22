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

import com.sun.net.httpserver.HttpServer;
import org.apache.http.HttpStatus;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.TagService;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SwaggerDocManagerImplTest {

    @InjectMocks
    private PullSwaggerDocServiceImpl serviceDocManager;

    @Mock
    private Set<UpstreamInstance> currentServices;

    @Mock
    private TagService tagService;

    @Mock
    private DocManager docManager;

    @Test
    public void testPullApiDocument() {
        serviceDocManager.pullApiDocument(currentServices);
        verify(currentServices).forEach(any());
    }

    @Test
    public void testPullApiDocumentSkipsLockWhenAlreadyRefreshed() {
        UpstreamInstance instance = new UpstreamInstance();
        instance.setContextPath("/test");
        instance.setStartupTime(1000L);
        TagDO.TagExt tagExt = new TagDO.TagExt();
        tagExt.setRefreshTime(1000L);
        tagExt.setApiDocMd5("existing-md5");
        TagVO tagVO = new TagVO();
        tagVO.setId("tag-id");
        tagVO.setExt(GsonUtils.getInstance().toJson(tagExt));
        when(tagService.findByQuery(instance.getContextPath(), AdminConstants.TAG_ROOT_PARENT_ID))
                .thenReturn(Collections.singletonList(tagVO));

        for (int i = 0; i < 100; i++) {
            serviceDocManager.pullApiDocument(instance);
        }

        verify(tagService, never()).updateTagExt(any(), any());
        verifyNoInteractions(docManager);
    }

    @Test
    public void testPullApiDocumentReleasesExpiredLockAfterHttpFailure() throws IOException {
        UpstreamInstance instance = new UpstreamInstance();
        instance.setContextPath("/test");
        instance.setIp("127.0.0.1");
        instance.setStartupTime(31001L);
        TagDO.TagExt tagExt = new TagDO.TagExt();
        tagExt.setRefreshTime(1000L);
        tagExt.setDocLock("1");
        TagVO tagVO = new TagVO();
        tagVO.setId("tag-id");
        tagVO.setExt(GsonUtils.getInstance().toJson(tagExt));
        when(tagService.findByQuery(instance.getContextPath(), AdminConstants.TAG_ROOT_PARENT_ID))
                .thenReturn(Collections.singletonList(tagVO));
        when(tagService.updateTagExt(eq(tagVO.getId()), any(TagDO.TagExt.class))).thenAnswer(invocation -> {
            TagDO.TagExt updatedTagExt = invocation.getArgument(1);
            // Serialize each write to distinguish persisted changes from in-memory mutations.
            tagVO.setExt(GsonUtils.getInstance().toJson(updatedTagExt));
            return 1;
        });

        AtomicInteger requests = new AtomicInteger();
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        try {
            server.createContext("/test/v3/api-docs", exchange -> {
                requests.incrementAndGet();
                exchange.sendResponseHeaders(HttpStatus.SC_SERVICE_UNAVAILABLE, -1);
                exchange.close();
            });
            server.start();
            instance.setPort(server.getAddress().getPort());

            serviceDocManager.pullApiDocument(instance);

            assertEquals(1, requests.get());
            verify(tagService, times(2)).updateTagExt(eq(tagVO.getId()), any(TagDO.TagExt.class));
            TagDO.TagExt persistedTagExt = GsonUtils.getInstance().fromJson(tagVO.getExt(), TagDO.TagExt.class);
            assertNull(persistedTagExt.getDocLock());
            assertEquals(tagExt.getRefreshTime(), persistedTagExt.getRefreshTime());
            verifyNoInteractions(docManager);
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void testPullApiDocumentDoesNotReleaseLockHeldByAnotherPull() {
        UpstreamInstance instance = new UpstreamInstance();
        instance.setContextPath("/test");
        instance.setStartupTime(31001L);
        TagDO.TagExt tagExt = new TagDO.TagExt();
        tagExt.setRefreshTime(1000L);
        tagExt.setDocLock(String.valueOf(Long.MAX_VALUE));
        TagVO tagVO = new TagVO();
        tagVO.setId("tag-id");
        tagVO.setExt(GsonUtils.getInstance().toJson(tagExt));
        when(tagService.findByQuery(instance.getContextPath(), AdminConstants.TAG_ROOT_PARENT_ID))
                .thenReturn(Collections.singletonList(tagVO));

        serviceDocManager.pullApiDocument(instance);

        verify(tagService, never()).updateTagExt(any(), any());
        verifyNoInteractions(docManager);
    }

}
