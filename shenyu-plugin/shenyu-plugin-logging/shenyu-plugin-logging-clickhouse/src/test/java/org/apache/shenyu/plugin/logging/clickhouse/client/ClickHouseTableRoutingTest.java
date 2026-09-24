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

import com.clickhouse.client.ClickHouseClient;
import com.clickhouse.client.ClickHouseClientBuilder;
import com.clickhouse.client.ClickHouseNode;
import com.clickhouse.client.ClickHouseRequest;
import com.clickhouse.client.ClickHouseValue;
import org.apache.shenyu.plugin.logging.clickhouse.config.ClickHouseLogCollectConfig.ClickHouseLogConfig;
import org.apache.shenyu.plugin.logging.clickhouse.constant.ClickHouseLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;

import java.util.Collections;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.RETURNS_SELF;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class ClickHouseTableRoutingTest {

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" ", "logs-cluster"})
    void routesDdlAndInsertsAccordingToClusterConfiguration(final String cluster) throws Exception {
        ClickHouseLogConfig config = new ClickHouseLogConfig();
        config.setHost("localhost");
        config.setPort("8123");
        config.setDatabase("logs");
        config.setUsername("default");
        config.setPassword("");
        config.setEngine("MergeTree");
        config.setClusterName(cluster);
        ClickHouseClient client = mock(ClickHouseClient.class);
        ClickHouseClientBuilder builder = mock(ClickHouseClientBuilder.class);
        ClickHouseRequest<?> request = mock(ClickHouseRequest.class, RETURNS_SELF);
        when(builder.build()).thenReturn(client);
        doReturn(request).when(client).connect(any(ClickHouseNode.class));
        boolean distributed = "logs-cluster".equals(cluster);
        String ddl = String.format(ClickHouseLoggingConstant.CREATE_DISTRIBUTED_TABLE_SQL, "logs", "logs", cluster, "logs");
        String insert = String.format(distributed ? ClickHouseLoggingConstant.PRE_INSERT_SQL : ClickHouseLoggingConstant.LOCAL_PRE_INSERT_SQL, "logs");
        try (MockedStatic<ClickHouseClient> clients = mockStatic(ClickHouseClient.class)) {
            clients.when(ClickHouseClient::builder).thenReturn(builder);
            clients.when(() -> ClickHouseClient.send(any(ClickHouseNode.class), anyString(), any(ClickHouseValue[].class), any(Object[][].class)))
                    .thenReturn(CompletableFuture.completedFuture(Collections.emptyList()));
            ClickHouseLogCollectClient collector = new ClickHouseLogCollectClient();
            try {
                assertTrue(collector.initClient0(config));
                verify(request).query(String.format(ClickHouseLoggingConstant.CREATE_DATABASE_SQL, "logs"));
                verify(request).query(String.format(ClickHouseLoggingConstant.CREATE_TABLE_SQL, "logs", "MergeTree", "30"));
                if (distributed) {
                    verify(request).query(ddl);
                } else {
                    verify(request, never()).query(ddl);
                }
                ShenyuRequestLog log = new ShenyuRequestLog();
                log.setTimeLocal("2026-09-25 00:00:00.123");
                collector.consume0(Collections.singletonList(log));
                clients.verify(() -> ClickHouseClient.send(any(ClickHouseNode.class), eq(insert), any(ClickHouseValue[].class), any(Object[][].class)));
            } finally {
                collector.close0();
            }
            verify(client).close();
        }
    }
}
