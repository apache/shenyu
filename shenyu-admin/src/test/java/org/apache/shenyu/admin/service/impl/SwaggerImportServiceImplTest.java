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

package org.apache.shenyu.admin.service.impl;

import okhttp3.MediaType;
import okhttp3.Protocol;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.Buffer;
import okio.BufferedSource;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.model.dto.SwaggerImportRequest;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test for {@link SwaggerImportServiceImpl}.
 */
public class SwaggerImportServiceImplTest {

    private static final String SWAGGER_URL = "https://8.8.8.8/swagger.json";

    private static final String SWAGGER_JSON = "{\"swagger\":\"2.0\",\"info\":{\"title\":\"test\",\"version\":\"1.0\"},\"paths\":{}}";

    private static final MediaType JSON_UTF_8 = MediaType.parse("application/json; charset=utf-8");

    private RecordingDocManager docManager;

    private StubHttpUtils httpUtils;

    @BeforeEach
    public void setUp() {
        docManager = new RecordingDocManager();
        httpUtils = new StubHttpUtils();
    }

    @Test
    public void importSwaggerShouldReadSmallSwaggerBody() throws IOException {
        SwaggerImportServiceImpl service = new SwaggerImportServiceImpl(docManager, httpUtils);
        httpUtils.setResponse(response(responseBody(
                SWAGGER_JSON, SWAGGER_JSON.getBytes(StandardCharsets.UTF_8).length, JSON_UTF_8)));

        String result = service.importSwagger(request());

        assertEquals("Import successful, supports Swagger 2.0 and OpenAPI 3.0 formats", result);
        assertEquals(SWAGGER_JSON, docManager.getDocJson());
    }

    @Test
    public void importSwaggerShouldRejectKnownContentLengthGreaterThanLimit() throws IOException {
        SwaggerImportServiceImpl service = new SwaggerImportServiceImpl(docManager, httpUtils);
        ReflectionTestUtils.setField(service, "maxSwaggerBodySize", 10L);
        httpUtils.setResponse(response(responseBody("small", 11L, JSON_UTF_8)));

        assertThrows(IllegalArgumentException.class, () -> service.importSwagger(request()));
    }

    @Test
    public void importSwaggerShouldRejectUnknownContentLengthWhenActualBodyExceedsLimit() throws IOException {
        SwaggerImportServiceImpl service = new SwaggerImportServiceImpl(docManager, httpUtils);
        ReflectionTestUtils.setField(service, "maxSwaggerBodySize", 10L);
        httpUtils.setResponse(response(responseBody("01234567890", -1L, JSON_UTF_8)));

        assertThrows(IllegalArgumentException.class, () -> service.importSwagger(request()));
    }

    @Test
    public void readLimitedResponseBodyShouldAllowBodyExactlyEqualToLimit() throws IOException {
        String body = "0123456789";

        String result = ReflectionTestUtils.invokeMethod(new SwaggerImportServiceImpl(docManager, httpUtils),
                "readLimitedResponseBody", responseBody(body, -1L, JSON_UTF_8), 10L);

        assertEquals(body, result);
    }

    @Test
    public void readLimitedResponseBodyShouldHandleNullBodySafely() {
        SwaggerImportServiceImpl service = new SwaggerImportServiceImpl(docManager, httpUtils);

        assertThrows(IllegalArgumentException.class, () ->
                ReflectionTestUtils.invokeMethod(service, "readLimitedResponseBody", null, 10L));
    }

    @Test
    public void readLimitedResponseBodyShouldUseResponseCharset() throws IOException {
        Charset charset = StandardCharsets.ISO_8859_1;
        byte[] bytes = new byte[] {'c', 'a', 'f', (byte) 0xE9};
        String body = new String(bytes, charset);
        ResponseBody responseBody = responseBody(bytes, -1L, MediaType.parse("text/plain; charset=iso-8859-1"));

        String result = ReflectionTestUtils.invokeMethod(new SwaggerImportServiceImpl(docManager, httpUtils),
                "readLimitedResponseBody", responseBody, 10L);

        assertEquals(body, result);
    }

    private SwaggerImportRequest request() {
        SwaggerImportRequest request = new SwaggerImportRequest();
        request.setSwaggerUrl(SWAGGER_URL);
        request.setProjectName("test");
        return request;
    }

    private static Response response(final ResponseBody body) {
        return new Response.Builder()
                .request(new Request.Builder().url(SWAGGER_URL).build())
                .protocol(Protocol.HTTP_1_1)
                .code(200)
                .message("OK")
                .body(body)
                .build();
    }

    private static ResponseBody responseBody(final String body, final long contentLength, final MediaType mediaType) {
        return responseBody(body, contentLength, mediaType, StandardCharsets.UTF_8);
    }

    private static ResponseBody responseBody(final String body, final long contentLength, final MediaType mediaType,
                                             final Charset charset) {
        return responseBody(body.getBytes(charset), contentLength, mediaType);
    }

    private static ResponseBody responseBody(final byte[] bytes, final long contentLength, final MediaType mediaType) {
        return new ResponseBody() {

            @Override
            public MediaType contentType() {
                return mediaType;
            }

            @Override
            public long contentLength() {
                return contentLength;
            }

            @Override
            public BufferedSource source() {
                return new Buffer().write(bytes);
            }
        };
    }

    private static final class RecordingDocManager implements DocManager {

        private String docJson;

        @Override
        public void addDocInfo(final UpstreamInstance instance, final String docJson, final String oldMd5,
                               final Consumer<DocInfo> callback) {
            this.docJson = docJson;
        }

        @Override
        public Collection<DocInfo> listAll() {
            return Collections.emptyList();
        }

        private String getDocJson() {
            return docJson;
        }
    }

    private static final class StubHttpUtils extends HttpUtils {

        private Response response;

        private void setResponse(final Response response) {
            this.response = response;
        }

        @Override
        public Response requestForResponse(final String url, final Map<String, ?> form,
                                           final Map<String, String> header, final HTTPMethod method,
                                           final boolean followRedirects) {
            return response;
        }
    }
}
