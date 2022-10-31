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

package org.apache.shenyu.plugin.logging.clickhouse.client;

import java.util.ArrayList;
import java.util.List;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.clickhouse.config.ClickHouseLogCollectConfig;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.junit.After;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;


/**
 * test cases for ClickHouseLogCollectClient.
 */
public class ClickHouseLogCollectClientTest {

    private final PluginData pluginData = new PluginData();

    private final List<ShenyuRequestLog> logs = new ArrayList<>();

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    private ClickHouseLogCollectConfig.ClickHouseLogConfig clickHouseLogConfig;

    private ClickHouseLogCollectClient clickHouseLogCollectClient;

    @BeforeEach
    public void setUp() {
        clickHouseLogCollectClient = new ClickHouseLogCollectClient();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"host\":\"127.0.0.1\",\"port\":\"8123\",\"database\":\"shenyu-gateway\",\"username\":\"foo\",\"password\":\"bar\"}");
        clickHouseLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), ClickHouseLogCollectConfig.ClickHouseLogConfig.class);
        shenyuRequestLog.setClientIp("127.0.0.1");
        shenyuRequestLog.setTimeLocal("2022-08-10 16:21:05.508");
        shenyuRequestLog.setMethod("/http/shenyu/client/post/hi");
        shenyuRequestLog.setRequestHeader("{\"content-length\":\"0\",\"Accept\":\"application/json\",\"Connection\":\"Keep-Alive\",\"User-Agent\":\"Apache-HttpClient/4.5.13 (Java/11.0.10)\","
            + "\"Host\":\"localhost:9195\",\"Accept-Encoding\":\"gzip,deflate\",\"Content-Type\":\"application/json\"}");
        shenyuRequestLog.setResponseHeader("{\"content-length\":\"65\",\"transfer-encoding\":\"chunked\",\"Content-Type\":\"application/json;charset=UTF-8\"}");
        shenyuRequestLog.setQueryParams("name=Tom");
        shenyuRequestLog.setRequestBody("");
        shenyuRequestLog.setRequestUri("http://localhost:9195/http/shenyu/client/post/hi?name=Tom");
        shenyuRequestLog.setResponseBody("[post method result]:hi! Tom! I'm Shenyu-Gateway System. Welcome!");
        shenyuRequestLog.setResponseContentLength(65);
        shenyuRequestLog.setRpcType("http");
        shenyuRequestLog.setStatus(200);
        shenyuRequestLog.setUpstreamIp("192.168.30.64");
        shenyuRequestLog.setUpstreamResponseTime(302L);
        shenyuRequestLog.setUserAgent("Apache-HttpClient/4.5.13 (Java/11.0.10)");
        shenyuRequestLog.setHost("localhost:9195");
        shenyuRequestLog.setModule("/http");
        shenyuRequestLog.setTraceId("");
        shenyuRequestLog.setPath("/http/shenyu/client/post/hi");
        logs.add(shenyuRequestLog);
    }

    @Test
    public void testConsume() {
        String msg = "";
        ClickHouseLogCollectConfig.INSTANCE.setClickHouseLogConfig(clickHouseLogConfig);
        clickHouseLogCollectClient.initClient(clickHouseLogConfig);
        try {
            clickHouseLogCollectClient.consume(logs);
        } catch (Exception e) {
            msg = "false";
        }
        Assertions.assertEquals(msg, "false");
    }

    @After
    public void clean() {
        clickHouseLogCollectClient.close();
    }

}
