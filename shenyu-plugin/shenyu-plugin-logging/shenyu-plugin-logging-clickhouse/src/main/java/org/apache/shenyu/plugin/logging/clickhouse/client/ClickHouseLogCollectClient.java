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
import com.clickhouse.client.ClickHouseCredentials;
import com.clickhouse.client.ClickHouseNode;
import com.clickhouse.client.ClickHouseRequest;
import com.clickhouse.client.ClickHouseFormat;
import com.clickhouse.client.ClickHouseProtocol;
import com.clickhouse.client.ClickHouseValue;
import com.clickhouse.client.data.ClickHouseIntegerValue;
import com.clickhouse.client.data.ClickHouseLongValue;
import com.clickhouse.client.data.ClickHouseOffsetDateTimeValue;
import com.clickhouse.client.data.ClickHouseStringValue;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.plugin.logging.clickhouse.config.ClickHouseLogCollectConfig;
import org.apache.shenyu.plugin.logging.clickhouse.constant.ClickHouseLoggingConstant;
import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.Objects;
import java.util.TimeZone;

/**
 * queue-based logging collector.
 */
public class ClickHouseLogCollectClient extends AbstractLogConsumeClient<ClickHouseLogCollectConfig.ClickHouseLogConfig, ShenyuRequestLog> {

    private ClickHouseClient client;

    private ClickHouseNode endpoint;

    private String database;

    @Override
    public void consume0(@NonNull final List<ShenyuRequestLog> logs) throws Exception {
        if (CollectionUtils.isNotEmpty(logs)) {
            Object[][] datas = new Object[logs.size()][];
            for (int i = 0; i < logs.size(); i++) {
                Object[] data = new Object[] {
                    DateUtils.parseLocalDateTime(logs.get(i).getTimeLocal(), DateUtils.DATE_FORMAT_DATETIME_MILLISECOND),
                    logs.get(i).getClientIp(),
                    logs.get(i).getMethod(),
                    logs.get(i).getRequestHeader(),
                    logs.get(i).getResponseHeader(),
                    logs.get(i).getQueryParams(),
                    logs.get(i).getRequestBody(),
                    logs.get(i).getRequestUri(),
                    logs.get(i).getResponseBody(),
                    logs.get(i).getResponseContentLength(),
                    logs.get(i).getRpcType(),
                    logs.get(i).getStatus(),
                    logs.get(i).getUpstreamIp(),
                    logs.get(i).getUpstreamResponseTime(),
                    logs.get(i).getUserAgent(),
                    logs.get(i).getHost(),
                    logs.get(i).getModule(),
                    logs.get(i).getTraceId(),
                    logs.get(i).getPath(),
                };
                datas[i] = data;
            }
            ClickHouseClient.send(endpoint, String.format(ClickHouseLoggingConstant.PRE_INSERT_SQL, database),
                    new ClickHouseValue[]{
                            ClickHouseOffsetDateTimeValue.ofNull(3, TimeZone.getTimeZone("Asia/Shanghai")),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseIntegerValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseIntegerValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseLongValue.ofNull(false),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                            ClickHouseStringValue.ofNull(),
                    }, datas).get();
        }
    }

    @Override
    public void close0() {
        if (Objects.nonNull(client)) {
            client.close();
        }
    }

    /**
     * init client .
     *
     * @param config properties.
     */
    @Override
    public void initClient0(@NonNull final ClickHouseLogCollectConfig.ClickHouseLogConfig config) {
        final String username = config.getUsername();
        final String password = config.getPassword();
        database = config.getDatabase();
        endpoint = ClickHouseNode.builder()
            .host(config.getHost())
            .port(ClickHouseProtocol.HTTP, Integer.valueOf(config.getPort()))
            .credentials(ClickHouseCredentials.fromUserAndPassword(username, password))
            .build();
        try {
            client = ClickHouseClient.builder().build();
            ClickHouseRequest<?> request = client.connect(endpoint).format(ClickHouseFormat.TabSeparatedWithNamesAndTypes);
            request.query(String.format(ClickHouseLoggingConstant.CREATE_DATABASE_SQL, database)).executeAndWait();
            request.query(String.format(ClickHouseLoggingConstant.CREATE_TABLE_SQL, database, config.getEngine())).executeAndWait();
            request.query(String.format(ClickHouseLoggingConstant.CREATE_DISTRIBUTED_TABLE_SQL, database, database, config.getClusterName(), database)).executeAndWait();
        } catch (Exception e) {
            LOG.error("inti ClickHouseLogClient error", e);
        }
    }
}
