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

package org.apache.shenyu.e2e.engine.handler;

import java.util.HashMap;
import java.util.Map;

/**
 * Gateway DataSyn Handler.
 */
public class GatewayDataSynHandler {

    private static final Map<String, Map<String, Object>> DATA_SYN_MAP = new HashMap<>();

    private static final Map<String, Object> WEBSOCKET_MAP = new HashMap<>();

    private static final Map<String, Object> ZOOKEEPER_MAP = new HashMap<>();

    private static final Map<String, Object> APOLLO_MAP = new HashMap<>();

    private static final Map<String, Object> HTTP_MAP = new HashMap<>();

    private static final Map<String, Object> NACOS_MAP = new HashMap<>();

    private static final Map<String, Object> ACM_MAP = new HashMap<>();

    public static void init() {
        DATA_SYN_MAP.put("websocket", WEBSOCKET_MAP);
        DATA_SYN_MAP.put("zookeeper", ZOOKEEPER_MAP);
        DATA_SYN_MAP.put("apollo", APOLLO_MAP);
        DATA_SYN_MAP.put("http", HTTP_MAP);
        DATA_SYN_MAP.put("nacos", NACOS_MAP);

        WEBSOCKET_MAP.put("urls", "ws://localhost:9095/websocket");

        ZOOKEEPER_MAP.put("url", "localhost:2181");
        ZOOKEEPER_MAP.put("sessionTimeout", "5000");
        ZOOKEEPER_MAP.put("connectionTimeout", "2000");

        HTTP_MAP.put("url", "http://localhost:9095");

        NACOS_MAP.put("url", "localhost:8848");
        NACOS_MAP.put("namespace", "1c10d748-af86-43b9-8265-75f487d20c6c");
        NACOS_MAP.put("username", "");
        NACOS_MAP.put("password", "");
        ACM_MAP.put("enabled", "false");
        ACM_MAP.put("endpoint", "acm.aliyun.com");
        ACM_MAP.put("namespace", "");
        ACM_MAP.put("accessKey", "");
        ACM_MAP.put("secretKey", "");
        NACOS_MAP.put("acm", ACM_MAP);

        APOLLO_MAP.put("appId", "shenyu");
        APOLLO_MAP.put("meta", "http://localhost:8080");
        APOLLO_MAP.put("env", "dev");
        APOLLO_MAP.put("clusterName", "test");
        APOLLO_MAP.put("namespace", "application");
    }

    public static Map<String, Object> getDataSynMap(String dataSyn) {
        return DATA_SYN_MAP.get(dataSyn);
    }
}
