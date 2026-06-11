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
import com.google.common.collect.Lists;
import okhttp3.Call;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.shenyu.common.constant.HttpConstants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import wiremock.org.apache.hc.core5.http.ContentType;
import wiremock.org.apache.hc.core5.http.HttpHeaders;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * AccessTokenManagerTest.
 */
@ExtendWith(MockitoExtension.class)
public class AccessTokenManagerTest {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(AccessTokenManagerTest.class);

    private WireMockServer wireMockServer;

    private AccessTokenManager accessTokenManager;

    private HttpConfig httpConfig;

    private String accessToken;

    @Mock
    private OkHttpClient mockOkHttpClient;

    @Mock
    private Call mockCall;

    @Mock
    private Response mockResponse;

    @Mock
    private ResponseBody mockResponseBody;

    @BeforeEach
    public void before() {
        this.wireMockServer = new WireMockServer(
                options()
                        .extensions(mock(ResponseTemplateTransformer.class))
                        .dynamicPort());
        this.wireMockServer.start();
        wireMockServer.stubFor(get(urlPathEqualTo("/platform/login"))
                .willReturn(aResponse()
                        .withHeader(HttpHeaders.CONTENT_TYPE, ContentType.APPLICATION_JSON.toString())
                        .withBody(this.mockLoginResponseJson())
                        .withStatus(200))
        );

        this.httpConfig = new HttpConfig();
        httpConfig.setUrl(this.getMockServerUrl());
        // set http connection timeout
        httpConfig.setConnectionTimeout(3000);
        // set delay time
        httpConfig.setDelayTime(3);
        httpConfig.setPassword("123456");
        httpConfig.setUsername("admin");
        
        OkHttpClient okHttpClient = new OkHttpClient.Builder()
                .readTimeout(Duration.ofMillis(Objects.isNull(httpConfig.getReadTimeout()) ? (int) HttpConstants.CLIENT_POLLING_READ_TIMEOUT : httpConfig.getReadTimeout()))
                .connectTimeout(Duration.ofMillis(Objects.isNull(httpConfig.getConnectionTimeout()) ? HttpConstants.CLIENT_POLLING_CONNECT_TIMEOUT : httpConfig.getConnectionTimeout()))
                .writeTimeout(Duration.ofMillis(Objects.isNull(httpConfig.getWriteTimeout()) ? (int) HttpConstants.CLIENT_POLLING_WRITE_TIMEOUT : httpConfig.getWriteTimeout()))
                .build();

        this.accessTokenManager = new AccessTokenManager(okHttpClient, httpConfig);
    }

    @Test
    public void testLogin() {
        accessTokenManager.login(Lists.newArrayList(httpConfig.getUrl().split(",")));
        assertEquals(this.accessToken, accessTokenManager.getAccessToken());
    }

    @Test
    public void testDoLoginWithUrlEncodingAndSpecialCharacters() throws Exception {

        HttpConfig testConfig = new HttpConfig();
        testConfig.setUrl("http://localhost:8080");
        testConfig.setUsername("test user@domain.com");
        testConfig.setPassword("pass+word=123&456 789%test");

        Map<String, Object> tokenData = new HashMap<>();
        tokenData.put("token", "special-token");
        tokenData.put("expiredTime", 3600L);
        
        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("data", tokenData);
        responseMap.put("code", CommonErrorCode.SUCCESSFUL);
        String responseJson = GsonUtils.getInstance().toJson(responseMap);

        when(mockOkHttpClient.newCall(any(Request.class))).thenReturn(mockCall);
        when(mockCall.execute()).thenReturn(mockResponse);
        when(mockResponse.isSuccessful()).thenReturn(true);
        when(mockResponse.body()).thenReturn(mockResponseBody);
        when(mockResponseBody.string()).thenReturn(responseJson);

        AccessTokenManager testManager = createTestAccessTokenManager(mockOkHttpClient, testConfig);

        Method doLoginMethod = AccessTokenManager.class.getDeclaredMethod("doLogin", String.class);
        doLoginMethod.setAccessible(true);
        Boolean result = (Boolean) doLoginMethod.invoke(testManager, "http://localhost:8080");

        assertTrue(result);
        assertEquals("special-token", testManager.getAccessToken());

        verify(mockOkHttpClient, times(2)).newCall(argThat(request -> {

            String url = request.url().toString();

            try {
                String expectedUsername = URLEncoder.encode("test user@domain.com", StandardCharsets.UTF_8);
                String expectedPassword = URLEncoder.encode("pass+word=123&456 789%test", StandardCharsets.UTF_8);

                return url.contains("userName=" + expectedUsername)
                        && url.contains("password=" + expectedPassword);
            } catch (Exception e) {
                LOG.error("URL encoding test failed", e);
                return false;
            }
        }));
    }

    @Test
    public void testDoLoginWithEncryptionPreservesOriginalPassword() throws Exception {

        HttpConfig testConfig = new HttpConfig();

        testConfig.setUrl("http://localhost:8080");
        testConfig.setUsername("admin");
        testConfig.setPassword("original+password");
        testConfig.setAesSecretKey("1234567890123456");
        testConfig.setAesSecretIv("1234567890123456");
        
        final String originalPassword = testConfig.getPassword();

        Map<String, Object> tokenData = new HashMap<>();
        tokenData.put("token", "encrypted-token");
        tokenData.put("expiredTime", 3600L);
        
        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("data", tokenData);
        responseMap.put("code", CommonErrorCode.SUCCESSFUL);
        String responseJson = GsonUtils.getInstance().toJson(responseMap);

        when(mockOkHttpClient.newCall(any(Request.class))).thenReturn(mockCall);
        when(mockCall.execute()).thenReturn(mockResponse);
        when(mockResponse.isSuccessful()).thenReturn(true);
        when(mockResponse.body()).thenReturn(mockResponseBody);
        when(mockResponseBody.string()).thenReturn(responseJson);
        
        AccessTokenManager testManager = createTestAccessTokenManager(mockOkHttpClient, testConfig);

        Method doLoginMethod = AccessTokenManager.class.getDeclaredMethod("doLogin", String.class);
        doLoginMethod.setAccessible(true);
        Boolean result1 = (Boolean) doLoginMethod.invoke(testManager, "http://localhost:8080");

        assertTrue(result1);
        assertEquals("encrypted-token", testManager.getAccessToken());

        // check password overwrite
        assertEquals(originalPassword, testConfig.getPassword());

        Boolean result2 = (Boolean) doLoginMethod.invoke(testManager, "http://localhost:8080");
        assertTrue(result2);

        assertEquals(originalPassword, testConfig.getPassword());
    }

    @Test
    public void testDoLoginFailureWhenResponseNotSuccessful() throws Exception {

        HttpConfig testConfig = new HttpConfig();
        testConfig.setUrl("http://localhost:8080");
        testConfig.setUsername("admin");
        testConfig.setPassword("password");

        when(mockOkHttpClient.newCall(any(Request.class))).thenReturn(mockCall);
        when(mockCall.execute()).thenReturn(mockResponse);
        when(mockResponse.isSuccessful()).thenReturn(false);
        
        AccessTokenManager testManager = createTestAccessTokenManager(mockOkHttpClient, testConfig);

        Method doLoginMethod = AccessTokenManager.class.getDeclaredMethod("doLogin", String.class);
        doLoginMethod.setAccessible(true);
        Boolean result = (Boolean) doLoginMethod.invoke(testManager, "http://localhost:8080");

        assertFalse(result);
        assertNull(testManager.getAccessToken());
    }

    @Test
    public void testDoLoginIOException() throws Exception {

        HttpConfig testConfig = new HttpConfig();

        testConfig.setUrl("http://localhost:8080");
        testConfig.setUsername("admin");
        testConfig.setPassword("password");

        when(mockOkHttpClient.newCall(any(Request.class))).thenReturn(mockCall);
        when(mockCall.execute()).thenThrow(new IOException("Network error"));
        
        AccessTokenManager testManager = createTestAccessTokenManager(mockOkHttpClient, testConfig);

        Method doLoginMethod = AccessTokenManager.class.getDeclaredMethod("doLogin", String.class);
        doLoginMethod.setAccessible(true);
        Boolean result = (Boolean) doLoginMethod.invoke(testManager, "http://localhost:8080");

        assertFalse(result);
        assertNull(testManager.getAccessToken());
    }

    /**
     * For testing only, create AccessTokenManager without starting the scheduled task.
     */
    private AccessTokenManager createTestAccessTokenManager(final OkHttpClient okHttpClient, final HttpConfig config) {
        return new AccessTokenManager(okHttpClient, config) {

            private void start(final List<String> servers) {
            }
        };
    }

    // mock configs fetch api response
    private String mockLoginResponseJson() {
        Map<String, Object> data = new HashMap<>();
        data.put("token", "token");
        this.accessToken = "token";
        data.put("expiredTime", 24 * 60 * 60 * 1000);
        Map<String, Object> result = new HashMap<>();
        result.put("data", data);
        result.put("code", CommonErrorCode.SUCCESSFUL);
        return GsonUtils.getInstance().toJson(result);
    }

    private String getMockServerUrl() {
        return "http://127.0.0.1:" + wireMockServer.port();
    }
}
