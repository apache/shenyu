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

package org.apache.shenyu.sync.data.http;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.extension.responsetemplating.ResponseTemplateTransformer;
import org.apache.shenyu.common.constant.HttpConstants;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.client.OkHttp3ClientHttpRequestFactory;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;
import wiremock.org.apache.http.HttpHeaders;
import wiremock.org.apache.http.entity.ContentType;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
public final class HttpSyncDataServiceTest {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(HttpSyncDataServiceTest.class);

    private WireMockServer wireMockServer;

    private PluginDataSubscriber pluginDataSubscriber;

    private MetaDataSubscriber metaDataSubscriber;

    private AuthDataSubscriber authDataSubscriber;

    private HttpSyncDataService httpSyncDataService;

    @BeforeEach
    public void before() {
        this.wireMockServer = new WireMockServer(
                options()
                        .extensions(new ResponseTemplateTransformer(false))
                        .dynamicPort());
        this.wireMockServer.start();
        wireMockServer.stubFor(get(urlPathEqualTo("/platform/login"))
                .willReturn(aResponse()
                        .withHeader(HttpHeaders.CONTENT_TYPE, ContentType.APPLICATION_JSON.toString())
                        .withBody(this.mockLoginResponseJson())
                        .withStatus(200))
        );
        wireMockServer.stubFor(get(urlPathEqualTo("/configs/fetch"))
                .willReturn(aResponse()
                        .withHeader(HttpHeaders.CONTENT_TYPE, ContentType.APPLICATION_JSON.toString())
                        .withBody(this.mockConfigsFetchResponseJson())
                        .withStatus(200))
        );
        wireMockServer.stubFor(post(urlPathEqualTo("/configs/listener"))
                .willReturn(aResponse()
                        .withHeader(HttpHeaders.CONTENT_TYPE, ContentType.APPLICATION_JSON.toString())
                        .withBody(this.mockConfigsListenResponseJson())
                        .withStatus(200))
        );

        HttpConfig httpConfig = new HttpConfig();
        httpConfig.setUrl(this.getMockServerUrl());
        // set http connection timeout
        httpConfig.setConnectionTimeout(3000);
        // set delay time
        httpConfig.setDelayTime(3);
        httpConfig.setPassword("123456");
        httpConfig.setUsername("admin");
        this.pluginDataSubscriber = mock(PluginDataSubscriber.class);
        this.metaDataSubscriber = mock(MetaDataSubscriber.class);
        this.authDataSubscriber = mock(AuthDataSubscriber.class);

        OkHttp3ClientHttpRequestFactory factory = new OkHttp3ClientHttpRequestFactory();
        factory.setConnectTimeout(Objects.isNull(httpConfig.getConnectionTimeout()) ? (int) HttpConstants.CLIENT_POLLING_CONNECT_TIMEOUT : httpConfig.getConnectionTimeout());
        factory.setReadTimeout(Objects.isNull(httpConfig.getReadTimeout()) ? (int) HttpConstants.CLIENT_POLLING_READ_TIMEOUT : httpConfig.getReadTimeout());
        factory.setWriteTimeout(Objects.isNull(httpConfig.getWriteTimeout()) ? (int) HttpConstants.CLIENT_POLLING_WRITE_TIMEOUT : httpConfig.getWriteTimeout());
        RestTemplate restTemplate = new RestTemplate(factory);

        AccessTokenManager accessTokenManager = new AccessTokenManager(restTemplate, httpConfig);
        this.httpSyncDataService = new HttpSyncDataService(httpConfig, pluginDataSubscriber, restTemplate,
                Collections.singletonList(metaDataSubscriber), Collections.singletonList(authDataSubscriber), accessTokenManager);
    }

    @AfterEach
    public void after() {
        try {
            httpSyncDataService.close();
        } catch (Exception e) {
            LOG.error(e.getMessage(), e);
        }
        AtomicBoolean running = (AtomicBoolean) ReflectionTestUtils.getField(httpSyncDataService, "RUNNING");
        assertFalse(Objects.requireNonNull(running).get());
    }

    @Test
    public void test() {
        AtomicBoolean running = (AtomicBoolean) ReflectionTestUtils.getField(httpSyncDataService, "RUNNING");
        assertTrue(Objects.requireNonNull(running).get());

        verify(pluginDataSubscriber, atLeastOnce()).refreshPluginDataAll();
        verify(metaDataSubscriber, atLeastOnce()).refresh();
        verify(authDataSubscriber, atLeastOnce()).refresh();
    }

    private String getMockServerUrl() {
        return "http://127.0.0.1:" + wireMockServer.port();
    }

    // mock configs listen api response
    private String mockConfigsListenResponseJson() {
        return "{\"code\":200,\"message\":\"success\",\"data\":[\"PLUGIN\"]}";
    }

    // mock configs fetch api response
    private String mockConfigsFetchResponseJson() {
        ConfigData<?> emptyData = new ConfigData<>()
                .setLastModifyTime(System.currentTimeMillis()).setData(Collections.emptyList())
                .setMd5("d751713988987e9331980363e24189cf");
        ConfigData<?> pluginData = new ConfigData<>()
                .setLastModifyTime(System.currentTimeMillis())
                .setData(Collections.singletonList(PluginData.builder()
                        .id("9")
                        .name("hystrix")
                        .role("0")
                        .enabled(false)
                        .build()))
                .setMd5("1298d5a533d0f896c60cbeca1ec7b017");
        Map<String, Object> data = new HashMap<>();
        data.put(ConfigGroupEnum.PLUGIN.name(), pluginData);
        data.put(ConfigGroupEnum.META_DATA.name(), emptyData);
        data.put(ConfigGroupEnum.APP_AUTH.name(), emptyData);
        data.put(ConfigGroupEnum.SELECTOR.name(), emptyData);
        data.put(ConfigGroupEnum.RULE.name(), emptyData);
        Map<String, Object> response = new HashMap<>();
        response.put("data", data);
        response.put("code", 200);
        return GsonUtils.getInstance().toJson(response);
    }

    // mock configs fetch api response
    private String mockLoginResponseJson() {
        Map<String, Object> result = new HashMap<>();
        Map<String, Object> data = new HashMap<>();
        data.put("token", "token");
        data.put("expiredTime", 24 * 60 * 60 * 1000);
        result.put("data", data);
        result.put("code", CommonErrorCode.SUCCESSFUL);
        return GsonUtils.getInstance().toJson(result);
    }
}
