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

package org.apache.shenyu.plugin.wasm.base.gateway;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.MediaType;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Verifies that explicit local management enablement keeps the production
 * dispatch path and its key check, even when health dispatch matches every path.
 */
@SpringBootTest(classes = WasmGatewayTest.GatewayApplication.class, webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {"spring.main.web-application-type=reactive", "spring.main.allow-bean-definition-overriding=true",
                "shenyu.local.enabled=true",
                "shenyu.local.sha512Key=BA3253876AED6BC22D4A6FF53D8406C6AD864195ED144AB5C87621B6C233B548BAEAE6956DF346EC8C17F5EA10F35EE3CBC514797ED7DDD3145464E2A0BAB413",
                "shenyu.health.enabled=true", "shenyu.health.paths=/", "shenyu.alert.enabled=false"})
final class LocalManagementDispatchTest {

    private static final String LOCAL_KEY = "123456";

    private static final String PLUGIN_BODY = "{\"id\":\"local-gate-plugin\",\"name\":\"local-gate\",\"enabled\":true}";

    @LocalServerPort
    private int port;

    private WebTestClient client;

    @BeforeEach
    void setUp() {
        client = WebTestClient.bindToServer().baseUrl("http://localhost:" + port)
                .responseTimeout(Duration.ofSeconds(15)).build();
        BaseDataCache.getInstance().cleanPluginData();
    }

    @Test
    void missingKeyIsRejected() {
        client.post().uri("/shenyu/plugin/saveOrUpdate")
                .contentType(MediaType.APPLICATION_JSON)
                .bodyValue(PLUGIN_BODY)
                .exchange()
                .expectStatus().isForbidden();
    }

    @Test
    void correctKeyReachesTheLocalController() {
        client.post().uri("/shenyu/plugin/saveOrUpdate")
                .header(Constants.LOCAL_KEY, LOCAL_KEY)
                .contentType(MediaType.APPLICATION_JSON)
                .bodyValue(PLUGIN_BODY)
                .exchange()
                .expectStatus().isOk();
        assertNotNull(BaseDataCache.getInstance().obtainPluginData("local-gate"));
    }
}
