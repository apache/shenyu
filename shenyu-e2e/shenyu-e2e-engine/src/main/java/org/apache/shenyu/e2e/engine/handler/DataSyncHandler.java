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
public class DataSyncHandler {
    
    private static final Map<String, Map<String, Object>> DATA_SYNC_MAP = new HashMap<>();
    
    private static final Map<String, Object> ADMIN_WEBSOCKET_MAP = new HashMap<>();
    
    private static final Map<String, Object> GATEWAY_WEBSOCKET_MAP = new HashMap<>();
    
    private static final Map<String, Object> ZOOKEEPER_MAP = new HashMap<>();
    
    private static final Map<String, Object> ADMIN_APOLLO_MAP = new HashMap<>();
    
    private static final Map<String, Object> GATEWAY_APOLLO_MAP = new HashMap<>();
    
    private static final Map<String, Object> ADMIN_HTTP_MAP = new HashMap<>();
    
    private static final Map<String, Object> GATEWAY_HTTP_MAP = new HashMap<>();
    
    private static final Map<String, Object> NACOS_MAP = new HashMap<>();
    
    private static final Map<String, Object> ACM_MAP = new HashMap<>();
    
    /**
     * init data sync handler.
     */
    public static void init() {
        DATA_SYNC_MAP.put("admin_websocket", ADMIN_WEBSOCKET_MAP);
        DATA_SYNC_MAP.put("gateway_websocket", GATEWAY_WEBSOCKET_MAP);
        DATA_SYNC_MAP.put("zookeeper", ZOOKEEPER_MAP);
        DATA_SYNC_MAP.put("admin_apollo", ADMIN_APOLLO_MAP);
        DATA_SYNC_MAP.put("gateway_apollo", GATEWAY_APOLLO_MAP);
        DATA_SYNC_MAP.put("admin_http", ADMIN_HTTP_MAP);
        DATA_SYNC_MAP.put("gateway_http", GATEWAY_HTTP_MAP);
        DATA_SYNC_MAP.put("nacos", NACOS_MAP);

        ADMIN_WEBSOCKET_MAP.put("enabled", "true");
        GATEWAY_WEBSOCKET_MAP.put("urls", "ws://admin:9095/websocket");

        ZOOKEEPER_MAP.put("url", "zookeeper:2181");
        ZOOKEEPER_MAP.put("sessionTimeout", "5000");
        ZOOKEEPER_MAP.put("connectionTimeout", "2000");

        ADMIN_HTTP_MAP.put("enabled", "true");
        GATEWAY_HTTP_MAP.put("url", "http://admin:9095");
        GATEWAY_HTTP_MAP.put("username", "admin");
        GATEWAY_HTTP_MAP.put("password", "123456");

        NACOS_MAP.put("url", "nacos:8848");
        NACOS_MAP.put("namespace", "1c10d748-af86-43b9-8265-75f487d20c6c");
        NACOS_MAP.put("username", "");
        NACOS_MAP.put("password", "");
        ACM_MAP.put("enabled", "false");
        ACM_MAP.put("endpoint", "acm.aliyun.com");
        ACM_MAP.put("namespace", "");
        ACM_MAP.put("accessKey", "");
        ACM_MAP.put("secretKey", "");
        NACOS_MAP.put("acm", ACM_MAP);

        ADMIN_APOLLO_MAP.put("appId", "shenyu");
        ADMIN_APOLLO_MAP.put("meta", "http://apollo-configservice:8080");
        ADMIN_APOLLO_MAP.put("env", "dev");
        ADMIN_APOLLO_MAP.put("clusterName", "test");
        ADMIN_APOLLO_MAP.put("namespace", "application");
        ADMIN_APOLLO_MAP.put("portalUrl", "http://apollo-portal:8070");
        ADMIN_APOLLO_MAP.put("token", "38d8911b852cd84ea4fe8c05227d5d0014bc3a95b9da42b0771ee7d9915aa379");
        GATEWAY_APOLLO_MAP.put("appId", "shenyu");
        GATEWAY_APOLLO_MAP.put("meta", "http://apollo-configservice:8080");
        GATEWAY_APOLLO_MAP.put("env", "dev");
        GATEWAY_APOLLO_MAP.put("clusterName", "test");
        GATEWAY_APOLLO_MAP.put("namespace", "application");
    }
    
    /**
     * get data sync map.
     * @param dataSyn dataSyn
     * @return Map
     */
    public static Map<String, Object> getDataSynMap(final String dataSyn) {
        return DATA_SYNC_MAP.get(dataSyn);
    }
}
