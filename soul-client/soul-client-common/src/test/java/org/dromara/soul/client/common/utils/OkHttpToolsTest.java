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

import com.google.gson.Gson;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import org.dromara.soul.client.common.dto.HttpRegisterDTO;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;

/**
 * Test case for {@link OkHttpTools}.
 *
 * @author Young Bean
 */
public class OkHttpToolsTest {

    private MockWebServer server;

    private String url;

    private final String json = "{\"appName\":\"soul\"}";

    @Before
    public void before() throws IOException {
        server = new MockWebServer();
        server.start();
        url = server.url("").url().toString();
    }

    @After
    public void after() throws IOException {
        server.shutdown();
    }

    @Test
    public void testPostReturnString() throws IOException {

        server.enqueue(new MockResponse().setResponseCode(200).setBody(json));

        assertEquals(json, OkHttpTools.getInstance().post(url, json));

    }

    @Test
    public void testPostReturnClassT() throws IOException {

        server.enqueue(new MockResponse().setResponseCode(200).setBody(json));

        assertEquals(new Gson().fromJson(json, HttpRegisterDTO.class),
                OkHttpTools.getInstance().post(url, json, HttpRegisterDTO.class));

    }

    @Test
    public void testPostReturnMap() throws IOException {

        final Map<String, String> map = new HashMap<>();
        map.put("appName", "soul");

        server.enqueue(new MockResponse().setResponseCode(200).setBody(json));

        assertEquals(new Gson().toJson(map), OkHttpTools.getInstance().post(url, map));
    }

    @Test
    public void testGetGson() {
        assertEquals(new Gson().toJson(json), OkHttpTools.getInstance().getGson().toJson(json));
    }

}
