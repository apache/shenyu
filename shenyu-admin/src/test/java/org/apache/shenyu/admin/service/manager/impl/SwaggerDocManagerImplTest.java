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
import okhttp3.MediaType;
import okhttp3.Protocol;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.Buffer;
import okio.BufferedSource;
import org.apache.http.HttpStatus;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.TagService;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SwaggerDocManagerImplTest {

    private static final String SWAGGER_HOST = "http://10.0.0.8:8080";

    private static final String PREFIXED_SWAGGER_URL = SWAGGER_HOST + "/mcp/v3/api-docs";

    private static final String ROOT_SWAGGER_URL = SWAGGER_HOST + "/v3/api-docs";

    private static final String PREFIXED_DOC = "{\"openapi\":\"prefixed\"}";

    private static final String ROOT_DOC = "{\"openapi\":\"root\"}";

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

    @Test
    public void testPullApiDocumentImportsPrefixedSwaggerWhenPresent() throws IOException {
        final UpstreamInstance instance = newInstance("/mcp");
        final TagVO tagVO = stubTag(instance);
        final List<AtomicBoolean> closed = new ArrayList<>();
        final List<String> urls = stubSwagger(url -> {
            assertEquals(PREFIXED_SWAGGER_URL, url);
            return httpResponse(url, HttpStatus.SC_OK, PREFIXED_DOC, closed);
        });

        serviceDocManager.pullApiDocument(instance);

        assertEquals(Collections.singletonList(PREFIXED_SWAGGER_URL), urls);
        verify(docManager).addDocInfo(eq(instance), eq(PREFIXED_DOC), isNull(), any());
        assertTrue(tagVO.getTagExt().getRefreshTime() > 1000L);
        assertNull(tagVO.getTagExt().getDocLock());
        assertAllClosed(closed);
    }

    @Test
    public void testPullApiDocumentRetriesRootSwaggerWhenPrefixedPathIsNotFound() throws IOException {
        final UpstreamInstance instance = newInstance("/mcp");
        final TagVO tagVO = stubTag(instance);
        final List<AtomicBoolean> closed = new ArrayList<>();
        final List<String> urls = stubSwagger(url -> {
            if (PREFIXED_SWAGGER_URL.equals(url)) {
                return httpResponse(url, HttpStatus.SC_NOT_FOUND, "missing", closed);
            }
            return httpResponse(url, HttpStatus.SC_OK, ROOT_DOC, closed);
        });

        serviceDocManager.pullApiDocument(instance);

        assertEquals(List.of(PREFIXED_SWAGGER_URL, ROOT_SWAGGER_URL), urls);
        verify(docManager).addDocInfo(eq(instance), eq(ROOT_DOC), isNull(), any());
        assertTrue(tagVO.getTagExt().getRefreshTime() > 1000L);
        assertNull(tagVO.getTagExt().getDocLock());
        assertAllClosed(closed);
    }

    @Test
    public void testPullApiDocumentGivesUpWhenPrefixedAndRootSwaggerAreNotFound() throws IOException {
        final UpstreamInstance instance = newInstance("/mcp");
        final TagVO tagVO = stubTag(instance);
        final List<String> urls = stubSwagger(url -> httpResponse(url, HttpStatus.SC_NOT_FOUND, "missing", new ArrayList<>()));

        serviceDocManager.pullApiDocument(instance);

        assertEquals(List.of(PREFIXED_SWAGGER_URL, ROOT_SWAGGER_URL), urls);
        assertPullFinishedWithoutImport(tagVO);
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "/"})
    public void testPullApiDocumentDoesNotRetryWhenContextPathIsEmptyOrRoot(final String contextPath) throws IOException {
        final UpstreamInstance instance = newInstance(contextPath);
        final TagVO tagVO = stubTag(instance);
        final List<String> urls = stubSwagger(url -> httpResponse(url, HttpStatus.SC_NOT_FOUND, "missing", new ArrayList<>()));

        serviceDocManager.pullApiDocument(instance);

        assertEquals(Collections.singletonList(ROOT_SWAGGER_URL), urls);
        assertPullFinishedWithoutImport(tagVO);
    }

    @ParameterizedTest
    @ValueSource(ints = {HttpStatus.SC_UNAUTHORIZED, HttpStatus.SC_INTERNAL_SERVER_ERROR})
    public void testPullApiDocumentDoesNotRetryRootOnNonNotFoundStatus(final int status) throws IOException {
        final UpstreamInstance instance = newInstance("/mcp");
        final TagVO tagVO = stubTag(instance);
        final List<AtomicBoolean> closed = new ArrayList<>();
        final List<String> urls = stubSwagger(url -> httpResponse(url, status, "denied", closed));

        serviceDocManager.pullApiDocument(instance);

        assertEquals(Collections.singletonList(PREFIXED_SWAGGER_URL), urls);
        assertPullFinishedWithoutImport(tagVO);
        assertAllClosed(closed);
    }

    @Test
    public void testPullApiDocumentDoesNotRetryRootWhenPrefixedRequestFails() throws IOException {
        final UpstreamInstance instance = newInstance("/mcp");
        final TagVO tagVO = stubTag(instance);
        final HttpUtils httpUtils = mock(HttpUtils.class);
        when(httpUtils.requestForResponse(anyString(), anyMap(), anyMap(), eq(HttpUtils.HTTPMethod.GET)))
                .thenThrow(new IOException("connection reset"));
        ReflectionTestUtils.setField(serviceDocManager, "httpUtils", httpUtils);
        ReflectionTestUtils.setField(serviceDocManager, "maxSwaggerBodySize", 1024L);

        serviceDocManager.pullApiDocument(instance);

        verify(httpUtils, times(1)).requestForResponse(eq(PREFIXED_SWAGGER_URL), anyMap(), anyMap(), eq(HttpUtils.HTTPMethod.GET));
        assertPullFinishedWithoutImport(tagVO);
    }

    private List<String> stubSwagger(final Function<String, Response> handler) throws IOException {
        List<String> urls = new ArrayList<>();
        HttpUtils httpUtils = mock(HttpUtils.class);
        when(httpUtils.requestForResponse(anyString(), anyMap(), anyMap(), eq(HttpUtils.HTTPMethod.GET)))
                .thenAnswer(invocation -> {
                    String url = invocation.getArgument(0);
                    urls.add(url);
                    if (urls.size() > 2) {
                        throw new IOException("unexpected extra swagger request " + url);
                    }
                    return handler.apply(url);
                });
        ReflectionTestUtils.setField(serviceDocManager, "httpUtils", httpUtils);
        ReflectionTestUtils.setField(serviceDocManager, "maxSwaggerBodySize", 1024L);
        return urls;
    }

    private TagVO stubTag(final UpstreamInstance instance) {
        TagDO.TagExt tagExt = new TagDO.TagExt();
        tagExt.setRefreshTime(1000L);
        TagVO tagVO = new TagVO();
        tagVO.setId("tag-id");
        tagVO.setExt(GsonUtils.getInstance().toJson(tagExt));
        when(tagService.findByQuery(instance.getContextPath(), AdminConstants.TAG_ROOT_PARENT_ID))
                .thenReturn(Collections.singletonList(tagVO));
        return tagVO;
    }

    private void assertPullFinishedWithoutImport(final TagVO tagVO) {
        verifyNoInteractions(docManager);
        assertNull(tagVO.getTagExt().getDocLock());
        assertEquals(Long.valueOf(1000L), tagVO.getTagExt().getRefreshTime());
        verify(tagService, times(2)).updateTagExt(eq(tagVO.getId()), any(TagDO.TagExt.class));
    }

    private static void assertAllClosed(final List<AtomicBoolean> closed) {
        assertTrue(closed.stream().allMatch(AtomicBoolean::get));
    }

    private static UpstreamInstance newInstance(final String contextPath) {
        UpstreamInstance instance = new UpstreamInstance();
        instance.setContextPath(contextPath);
        instance.setIp("10.0.0.8");
        instance.setPort(8080);
        instance.setStartupTime(31001L);
        return instance;
    }

    private static Response httpResponse(final String url, final int code, final String body, final List<AtomicBoolean> closed) {
        AtomicBoolean closedFlag = new AtomicBoolean();
        closed.add(closedFlag);
        ResponseBody responseBody = new ResponseBody() {

            @Override
            public MediaType contentType() {
                return MediaType.parse("application/json");
            }

            @Override
            public long contentLength() {
                return body.getBytes(StandardCharsets.UTF_8).length;
            }

            @Override
            public BufferedSource source() {
                return new Buffer().writeUtf8(body);
            }

            @Override
            public void close() {
                closedFlag.set(true);
                super.close();
            }
        };
        return new Response.Builder()
                .request(new Request.Builder().url(url).build())
                .protocol(Protocol.HTTP_1_1)
                .code(code)
                .message("test")
                .body(responseBody)
                .build();
    }

}
