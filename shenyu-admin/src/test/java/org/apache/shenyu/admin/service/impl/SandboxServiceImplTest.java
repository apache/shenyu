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

import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.Collections;

import org.apache.shenyu.admin.model.dto.ProxyGatewayDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import static org.assertj.core.api.Assertions.assertThat;
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

    @Test
    void requestProxyGatewayShouldCopyBody() throws IOException {
        HttpServer server = startHttpServer("proxied response");
        int port = server.getAddress().getPort();
        SandboxServiceImpl sandboxService = new SandboxServiceImpl(appAuthService, shenyuDictService);

        when(shenyuDictService.list(anyString())).thenReturn(Collections.singletonList(buildDict(port)));

        try {
            MockHttpServletResponse response = new MockHttpServletResponse();
            sandboxService.requestProxyGateway(buildProxyGatewayDTO(port), new MockHttpServletRequest(), response);

            assertThat(response.getContentAsString()).isEqualTo("proxied response");
        } finally {
            server.stop(0);
        }
    }

    @Test
    void requestProxyGatewayShouldNotExposeAppSecretInBeforeSignHeader() throws IOException {
        HttpServer server = startHttpServer("proxied response");
        int port = server.getAddress().getPort();
        SandboxServiceImpl sandboxService = new SandboxServiceImpl(appAuthService, shenyuDictService);

        when(shenyuDictService.list(anyString())).thenReturn(Collections.singletonList(buildDict(port)));

        String testAppKey = "testAppKey";
        String testAppSecret = "testSecret123";
        AppAuthDO appAuthDO = new AppAuthDO(testAppKey, testAppSecret, true, true, null, null, null, null);
        when(appAuthService.findByAppKey(testAppKey)).thenReturn(appAuthDO);

        try {
            MockHttpServletResponse response = new MockHttpServletResponse();
            ProxyGatewayDTO dto = buildProxyGatewayDTO(port);
            dto.setAppKey(testAppKey);
            sandboxService.requestProxyGateway(dto, new MockHttpServletRequest(), response);

            String beforesign = response.getHeader("sandbox-beforesign");
            assertThat(beforesign).doesNotContain(testAppSecret);

            String sign = response.getHeader("sandbox-sign");
            assertThat(sign).isNotNull();
        } finally {
            server.stop(0);
        }
    }

    private ProxyGatewayDTO buildProxyGatewayDTO(final int port) {
        ProxyGatewayDTO proxyGatewayDTO = new ProxyGatewayDTO();
        proxyGatewayDTO.setRequestUrl("http://localhost:" + port + "/proxy");
        proxyGatewayDTO.setHeaders(Collections.singletonMap("Content-Type", "application/x-www-form-urlencoded"));
        proxyGatewayDTO.setBizParam(Collections.emptyMap());
        return proxyGatewayDTO;
    }

    private ShenyuDictVO buildDict(final int port) {
        ShenyuDictVO dictVO = new ShenyuDictVO();
        dictVO.setDictValue("http://localhost:" + port);
        dictVO.setEnabled(Boolean.TRUE);
        return dictVO;
    }

    private HttpServer startHttpServer(final String content) throws IOException {
        byte[] responseBody = content.getBytes(StandardCharsets.UTF_8);
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/proxy", exchange -> {
            try {
                exchange.sendResponseHeaders(200, responseBody.length);
                try (OutputStream outputStream = exchange.getResponseBody()) {
                    outputStream.write(responseBody);
                }
            } finally {
                exchange.close();
            }
        });
        server.start();
        return server;
    }
}
