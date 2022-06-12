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

package org.apache.shenyu.springboot.starter.sync.data.http;

import com.github.tomakehurst.wiremock.WireMockServer;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.http.HttpSyncDataService;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.web.servlet.server.ServletWebServerFactory;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import wiremock.org.apache.http.HttpHeaders;
import wiremock.org.apache.http.entity.ContentType;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for {@link HttpSyncDataConfiguration}.
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest(
        classes = {
                HttpSyncDataConfiguration.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {
                "shenyu.sync.http.url=http://localhost:18848",
                "shenyu.sync.http.delayTime=3",
                "shenyu.sync.http.username=admin",
                "shenyu.sync.http.password=123456",
                "shenyu.sync.http.connectionTimeout=5",
                "spring.main.web-application-type=none"
        })
@EnableAutoConfiguration
@MockBean({PluginDataSubscriber.class, ServletWebServerFactory.class})
public final class HttpClientPluginConfigurationTest {

    @Autowired
    private HttpConfig httpConfig;

    @Autowired
    private HttpSyncDataService httpSyncDataService;

    @BeforeAll
    public static void setUpWireMock() throws Exception {
        WireMockServer wireMockServer = new WireMockServer(options().port(18848));

        wireMockServer.stubFor(get(urlPathEqualTo("/platform/login"))
                .willReturn(aResponse()
                        .withHeader(HttpHeaders.CONTENT_TYPE, ContentType.APPLICATION_JSON.toString())
                        .withBody(mockLoginResponseJson())
                        .withStatus(200))
        );

        wireMockServer.stubFor(get(urlPathEqualTo("/configs/fetch"))
                .willReturn(aResponse()
                        .withHeader(HttpHeaders.CONTENT_TYPE, ContentType.APPLICATION_JSON.toString())
                        .withBody(mockConfigsFetchResponseJson())
                        .withStatus(200))
        );
        wireMockServer.stubFor(post(urlPathEqualTo("/configs/listener"))
                .willReturn(aResponse()
                        .withHeader(HttpHeaders.CONTENT_TYPE, ContentType.APPLICATION_JSON.toString())
                        .withBody(mockConfigsListenResponseJson())
                        .withStatus(200))
        );

        wireMockServer.start();
    }

    @Test
    public void testHttpSyncDataService() {
        assertNotNull(httpSyncDataService);
    }

    @Test
    public void testHttpConfig() {
        assertThat(httpConfig.getUrl(), is("http://localhost:18848"));
        assertThat(httpConfig.getDelayTime(), is(3));
        assertThat(httpConfig.getConnectionTimeout(), is(5));
    }

    // mock configs listen api response
    private static String mockConfigsListenResponseJson() {
        return "{\"code\":200,\"message\":\"success\",\"data\":[\"PLUGIN\"]}";
    }

    // mock configs fetch api response
    private static String mockConfigsFetchResponseJson() throws Exception {
        return new String(Files.readAllBytes(
                Paths.get(Objects.requireNonNull(HttpClientPluginConfigurationTest.class.getClassLoader()
                        .getResource("mock_configs_fetch_response.json")).toURI())));
    }

    // mock configs fetch api response
    private static String mockLoginResponseJson() {
        Map<String, Object> result = new HashMap<>();
        Map<String, Object> data = new HashMap<>();
        data.put("token", "token");
        data.put("expiredTime", 24 * 60 * 60 * 1000);
        result.put("data", data);
        result.put("code", CommonErrorCode.SUCCESSFUL);
        return GsonUtils.getInstance().toJson(result);
    }
}
