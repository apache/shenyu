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

package org.dromara.soul.client.common.utils;

import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.dromara.soul.client.common.dto.HttpRegisterDTO;
import org.dromara.soul.common.utils.GsonUtils;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import wiremock.com.google.common.net.HttpHeaders;
import wiremock.org.apache.http.entity.ContentType;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

/**
 * Test case for {@link OkHttpTools}.
 *
 * @author Young Bean
 * @author dengliming
 */
public final class OkHttpToolsTest {

    @Rule
    public WireMockRule wireMockRule = new WireMockRule(WireMockConfiguration.wireMockConfig().dynamicPort(), false);

    private String url;

    private final String json = "{\"appName\":\"soul\"}";

    @Before
    public void setUpWireMock() {
        wireMockRule.stubFor(post(urlPathEqualTo("/"))
                .willReturn(aResponse()
                        .withHeader(HttpHeaders.CONTENT_TYPE, ContentType.APPLICATION_JSON.toString())
                        .withBody(json)
                        .withStatus(200))
        );
        url = "http://localhost:" + wireMockRule.port();
    }

    @Test
    public void testPostReturnString() throws IOException {
        assertThat(json, is(OkHttpTools.getInstance().post(url, json)));
    }

    @Test
    public void testPostReturnClassT() throws IOException {
        assertThat(GsonUtils.getInstance().fromJson(json, HttpRegisterDTO.class),
                equalTo(OkHttpTools.getInstance().post(url, json, HttpRegisterDTO.class)));
    }

    @Test
    public void testPostReturnMap() throws IOException {
        final Map<String, String> map = new HashMap<>();
        map.put("appName", "soul");
        assertThat(json, is(OkHttpTools.getInstance().post(url, map)));
    }

    @Test
    public void testGetGson() {
        assertThat(OkHttpTools.getInstance().getGson(), notNullValue());
    }
}
