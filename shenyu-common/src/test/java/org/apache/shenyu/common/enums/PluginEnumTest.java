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

package org.apache.shenyu.common.enums;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test Cases for PluginEnum.
 */
public final class PluginEnumTest {

    @Test
    public void testGetPluginEnumByName() {
        Arrays.stream(PluginEnum.values())
                .forEach(pluginEnum -> assertEquals(pluginEnum, PluginEnum.getPluginEnumByName(pluginEnum.getName())));
    }

    @Test
    public void testGetPluginEnumByNameInvalid() {
        assertEquals(PluginEnum.GLOBAL, PluginEnum.getPluginEnumByName("invalidName"));
    }

    @Test
    public void testGetUpstreamNames() {
        List<String> list = PluginEnum.getUpstreamNames();
        assert !list.isEmpty();
    }

    @Test
    public void testGetCode() {
        Arrays.stream(PluginEnum.values())
                .forEach(pluginEnum -> assertEquals(pluginEnum.getCode(), PluginEnum.getPluginEnumByName(pluginEnum.getName()).getCode()));
    }

    /**
     * Verify that PluginEnum.getCode() matches the sort values in db/init/mysql/schema.sql.
     * This prevents drift between the Java enum and database initialization scripts.
     *
     * @param pluginName plugin name
     * @param expectedSort expected sort value from schema.sql
     */
    @ParameterizedTest
    @CsvSource({
            "sign, 20",
            "waf, 50",
            "rateLimiter, 60",
            "contextPath, 80",
            "rewrite, 90",
            "cryptorRequest, 100",
            "redirect, 110",
            "request, 120",
            "generalContext, 125",
            "hystrix, 130",
            "sentinel, 140",
            "resilience4j, 310",
            "loggingConsole, 160",
            "loggingRocketMQ, 170",
            "loggingRabbitMQ, 171",
            "loggingAliyunSls, 175",
            "loggingTencentCls, 176",
            "loggingHuaweiLts, 177",
            "loggingKafka, 180",
            "loggingPulsar, 185",
            "loggingElasticSearch, 190",
            "loggingClickHouse, 195",
            "divide, 200",
            "websocket, 200",
            "modifyResponse, 220",
            "dubbo, 310",
            "sofa, 310",
            "tars, 310",
            "grpc, 310",
            "motan, 310",
            "cryptorResponse, 410",
            "mock, 1",
            "jwt, 30",
            "oauth2, 40",
            "casdoor, 40",
            "cache, 10",
            "mqtt, 125",
            "keyAuth, 150",
            "basicAuth, 150",
            "tcp, 320",
            "aiProxy, 199",
            "aiTokenLimiter, 171",
            "aiPrompt, 170",
            "aiRequestTransformer, 65",
            "aiResponseTransformer, 66",
            "mcpServer, 180"
    })
    public void pluginEnumSortShouldMatchDbSchema(final String pluginName, final int expectedSort) {
        PluginEnum pluginEnum = PluginEnum.getPluginEnumByName(pluginName);
        assertEquals(expectedSort, pluginEnum.getCode(),
                String.format("PluginEnum.%s sort value (%d) does not match DB schema.sql sort value (%d). "
                        + "Please keep PluginEnum and schema.sql in sync.", pluginEnum.name(), pluginEnum.getCode(), expectedSort));
    }

    @Test
    public void testGetRole() {
        Arrays.stream(PluginEnum.values())
                .forEach(pluginEnum -> assertEquals(pluginEnum.getRole(), PluginEnum.getPluginEnumByName(pluginEnum.getName()).getRole()));
    }
}
