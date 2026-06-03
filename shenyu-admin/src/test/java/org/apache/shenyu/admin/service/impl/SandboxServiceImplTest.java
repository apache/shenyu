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

import java.io.IOException;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicBoolean;

import okhttp3.MediaType;
import okhttp3.Protocol;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.Buffer;
import okio.BufferedSource;
import okio.ForwardingSource;
import okio.Okio;
import org.apache.shenyu.admin.model.dto.ProxyGatewayDTO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link SandboxServiceImpl}.
 */
@ExtendWith(MockitoExtension.class)
final class SandboxServiceImplTest {

    @Mock
    private AppAuthService appAuthService;

    @Mock
    private ShenyuDictService shenyuDictService;

    @Mock
    private HttpUtils httpUtils;

    @Test
    void requestProxyGatewayShouldCopyBodyAndCloseResponse() throws IOException {
        AtomicBoolean bodyClosed = new AtomicBoolean();
        Response proxyResponse = buildResponse("proxied response", bodyClosed);
        SandboxServiceImpl sandboxService = new SandboxServiceImpl(appAuthService, shenyuDictService, httpUtils);

        when(shenyuDictService.list(anyString())).thenReturn(Collections.singletonList(buildDict()));
        when(httpUtils.requestCall(anyString(), anyMap(), anyMap(), any(HttpUtils.HTTPMethod.class), anyList())).thenReturn(proxyResponse);

        MockHttpServletResponse response = new MockHttpServletResponse();
        sandboxService.requestProxyGateway(buildProxyGatewayDTO(), new MockHttpServletRequest(), response);

        assertThat(response.getContentAsString()).isEqualTo("proxied response");
        assertThat(bodyClosed).isTrue();
    }

    private ProxyGatewayDTO buildProxyGatewayDTO() {
        ProxyGatewayDTO proxyGatewayDTO = new ProxyGatewayDTO();
        proxyGatewayDTO.setRequestUrl("http://localhost:8080/proxy");
        proxyGatewayDTO.setHeaders(Collections.emptyMap());
        proxyGatewayDTO.setBizParam(Collections.emptyMap());
        return proxyGatewayDTO;
    }

    private ShenyuDictVO buildDict() {
        ShenyuDictVO dictVO = new ShenyuDictVO();
        dictVO.setDictValue("http://localhost:8080");
        dictVO.setEnabled(Boolean.TRUE);
        return dictVO;
    }

    private Response buildResponse(final String content, final AtomicBoolean bodyClosed) {
        return new Response.Builder()
                .request(new Request.Builder().url("http://localhost:8080/proxy").build())
                .protocol(Protocol.HTTP_1_1)
                .code(200)
                .message("OK")
                .body(buildResponseBody(content, bodyClosed))
                .build();
    }

    private ResponseBody buildResponseBody(final String content, final AtomicBoolean bodyClosed) {
        Buffer buffer = new Buffer().writeUtf8(content);
        BufferedSource source = Okio.buffer(new ForwardingSource(buffer) {
            @Override
            public void close() throws IOException {
                bodyClosed.set(true);
                super.close();
            }
        });
        return new ResponseBody() {
            @Override
            public MediaType contentType() {
                return MediaType.parse("text/plain");
            }

            @Override
            public long contentLength() {
                return content.length();
            }

            @Override
            public BufferedSource source() {
                return source;
            }
        };
    }
}
