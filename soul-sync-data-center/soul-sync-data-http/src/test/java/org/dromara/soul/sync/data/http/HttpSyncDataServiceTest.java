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

package org.dromara.soul.sync.data.http;

import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.dromara.soul.sync.data.http.config.HttpConfig;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import wiremock.org.apache.http.HttpHeaders;
import wiremock.org.apache.http.entity.ContentType;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;

/**
 * Test cases for {@link HttpSyncDataService}.
 *
 * @author davidliu
 */
public class HttpSyncDataServiceTest {
    
    @Rule
    public WireMockRule wireMockRule = new WireMockRule(WireMockConfiguration.wireMockConfig().dynamicPort(), false);
    
    @Before
    public final void setUpWiremock() {
        wireMockRule.stubFor(get(urlPathEqualTo("/configs/fetch"))
                .willReturn(aResponse()
                        .withHeader(HttpHeaders.CONTENT_TYPE, ContentType.APPLICATION_JSON.toString())
                        .withBody(this.mockConfigsFetchResponseJson())
                        .withStatus(200))
        );
        wireMockRule.stubFor(post(urlPathEqualTo("/configs/listener"))
                .willReturn(aResponse()
                        .withHeader(HttpHeaders.CONTENT_TYPE, ContentType.APPLICATION_JSON.toString())
                        .withBody(this.mockConfigsListenResponseJson())
                        .withStatus(200))
        );
    }
    
    /**
     * this method covers {@link HttpSyncDataService} constructor and {@link HttpSyncDataService#close()} method.
     *
     * @throws Exception any exception
     */
    @Test
    public void test() throws Exception {
        try (HttpSyncDataService ignored = this.buildHttpSyncDataService()) {
            // sleep 5 seconds to ensure Http Long polling task run
            TimeUnit.SECONDS.sleep(5);
        }
        
    }
    
    private HttpSyncDataService buildHttpSyncDataService() {
        HttpConfig httpConfig = new HttpConfig();
        httpConfig.setUrl(this.getMockServerUrl());
        // set http connection timeout
        httpConfig.setConnectionTimeout(3);
        // set delay time
        httpConfig.setDelayTime(3);
        PluginDataSubscriber pluginDataSubscriber = new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
            
            }
        };
        List<MetaDataSubscriber> metaDataSubscribers = Collections.singletonList(new MetaDataSubscriber() {
            @Override
            public void onSubscribe(final MetaData metaData) {
            
            }
            
            @Override
            public void unSubscribe(final MetaData metaData) {
            
            }
        });
        List<AuthDataSubscriber> authDataSubscribers = Collections.singletonList(new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
            
            }
            
            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
            
            }
        });
        return new HttpSyncDataService(httpConfig, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers);
    }
    
    private String getMockServerUrl() {
        return "http://localhost:" + wireMockRule.port();
    }
    
    // mock configs listen api response
    private String mockConfigsListenResponseJson() {
        return "{\"code\":200,\"message\":\"success\",\"data\":[\"PLUGIN\"]}";
    }
    
    // mock configs fetch api response
    private String mockConfigsFetchResponseJson() {
        try (FileInputStream fis = new FileInputStream(Objects.requireNonNull(this.getClass().getClassLoader().getResource("mock_configs_fetch_response.json")).getPath());
             InputStreamReader reader = new InputStreamReader(fis);
             BufferedReader bufferedReader = new BufferedReader(reader)
        ) {
            StringBuilder builder = new StringBuilder();
            bufferedReader.lines().forEach(builder::append);
            return builder.toString();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }
}
