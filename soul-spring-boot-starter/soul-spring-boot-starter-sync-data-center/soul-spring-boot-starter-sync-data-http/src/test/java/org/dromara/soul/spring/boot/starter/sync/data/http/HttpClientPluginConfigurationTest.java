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

package org.dromara.soul.spring.boot.starter.sync.data.http;

import okhttp3.mockwebserver.Dispatcher;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import okhttp3.mockwebserver.RecordedRequest;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.dromara.soul.sync.data.http.HttpSyncDataService;
import org.dromara.soul.sync.data.http.config.HttpConfig;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Objects;

/**
 * Test cases for {@link HttpSyncDataConfiguration}.
 *
 * @author strawberry-crisis
 */
@RunWith(SpringRunner.class)
@SpringBootTest(
        classes = {
                HttpSyncDataConfiguration.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {
                "soul.sync.http.url=http://localhost:18848",
                "soul.sync.http.delayTime=3",
                "soul.sync.http.connectionTimeout=5"
        })
@EnableAutoConfiguration
@MockBean(PluginDataSubscriber.class)
public class HttpClientPluginConfigurationTest {

    private static MockWebServer server;

    @Autowired
    private HttpConfig httpConfig;

    @Autowired
    private HttpSyncDataService httpSyncDataService;

    @BeforeClass
    public static void initMockServer() throws IOException {
        server = new MockWebServer();
        final TestDispatcher dispatcher = new TestDispatcher();
        server.setDispatcher(dispatcher);
        server.start(18848);
    }

    @Test
    public void testHttpSyncDataService() {
        Assert.assertNotNull(httpSyncDataService);
    }

    @Test
    public void testHttpConfig() {
        Assert.assertEquals("http://localhost:18848", httpConfig.getUrl());
        Assert.assertEquals(Integer.valueOf(3), httpConfig.getDelayTime());
        Assert.assertEquals(Integer.valueOf(5), httpConfig.getConnectionTimeout());
    }

    @AfterClass
    public static void closeMockWebServer() throws IOException {
        server.close();
    }

    private static class TestDispatcher extends Dispatcher {
        @Override
        public MockResponse dispatch(final RecordedRequest request) {
            String requestBodyContent = "";
            if (request.getPath().contains("/configs/fetch")) {
                requestBodyContent = Objects.requireNonNull(mockConfigsFetchResponseJson());
            } else if (request.getPath().contains("/configs/listener")) {
                requestBodyContent = mockConfigsListenResponseJson();
            }
            return new MockResponse()
                    .addHeader("Content-Type", MediaType.APPLICATION_JSON_VALUE)
                    .setBody(requestBodyContent)
                    .setResponseCode(200);
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
}
