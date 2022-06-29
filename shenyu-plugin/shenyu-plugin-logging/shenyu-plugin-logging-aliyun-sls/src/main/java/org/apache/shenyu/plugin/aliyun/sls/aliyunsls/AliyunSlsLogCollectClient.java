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

package org.apache.shenyu.plugin.aliyun.sls.aliyunsls;

import com.aliyun.openservices.log.Client;
import com.aliyun.openservices.log.common.LogItem;
import com.aliyun.openservices.log.common.LogStore;
import com.aliyun.openservices.log.exception.LogException;
import com.aliyun.openservices.log.response.CreateLogStoreResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.aliyun.sls.LogConsumeClient;
import org.apache.shenyu.plugin.aliyun.sls.constant.LoggingConstant;
import org.apache.shenyu.plugin.aliyun.sls.entity.ShenyuRequestLog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Aliyun sls log Collect client.
 */
public class AliyunSlsLogCollectClient implements LogConsumeClient {

    private static final Logger LOG = LoggerFactory.getLogger(AliyunSlsLogCollectClient.class);

    private Client client;

    private String projectName;

    private String logStore;

    private String topic;

    private final AtomicBoolean isStarted = new AtomicBoolean(false);

    /**
     * init aliyun sls client.
     *
     * @param props props
     */
    public void initClient(final Properties props) {
        if (MapUtils.isEmpty(props)) {
            LOG.error("RocketMQ props is empty. failed init RocketMQ producer");
            return;
        }
        if (isStarted.get()) {
            close();
        }
        String accessId = props.getProperty(LoggingConstant.ACCESS_ID);
        String accessKey = props.getProperty(LoggingConstant.ACCESS_KEY);
        String host = props.getProperty(LoggingConstant.HOST);
        if (StringUtils.isBlank(accessId) || StringUtils.isBlank(accessKey) || StringUtils.isBlank(host)) {
            LOG.error("init aliyun sls client error, please check accessId, accessKey or host");
            return;
        }
        client = new Client(host, accessId, accessKey);

        // create LogStore
        projectName = props.getProperty(LoggingConstant.PROJECT_NAME);
        logStore = props.getProperty(LoggingConstant.LOG_STORE);
        topic = props.getProperty(LoggingConstant.TOPIC);
        int ttlInDay = Integer.parseInt(props.getProperty(LoggingConstant.TTL_IN_DAY));
        int shardCount = Integer.parseInt(props.getProperty(LoggingConstant.SHARD_COUNT));
        LogStore store = new LogStore(logStore, ttlInDay, shardCount);
        try {
            isStarted.set(true);
            Runtime.getRuntime().addShutdownHook(new Thread(this::close));
            CreateLogStoreResponse res = client.CreateLogStore(projectName, store);
        } catch (LogException e) {
            LOG.error("error code:{}, error message:{}, error requestId:{}", e.GetErrorCode(), e.GetErrorMessage(), e.getRequestId());
        }
    }

    /**
     * aliyun sls consume.
     *
     * @param logs list of log
     */
    @Override
    public void consume(final List<ShenyuRequestLog> logs) {
        if (CollectionUtils.isEmpty(logs) || !isStarted.get()) {
            return;
        }

        logs.forEach(log -> {
            List<LogItem> logGroup = new ArrayList<LogItem>();
            LogItem logItem = new LogItem((int) (System.currentTimeMillis() / 1000));
            logItem.PushBack("level", "info");
            logItem.PushBack("name", log.getRequestUri());
            logItem.PushBack("message", GsonUtils.getGson().toJson(log));
            logGroup.add(logItem);
            try {
                client.PutLogs(projectName, logStore, topic, logGroup, "shenyu-gateway");
            } catch (LogException e) {
                LOG.error("error code :{}, error message :{}, error message :{}", e.GetErrorCode(), e.GetErrorMessage(), e.getRequestId());
            }
        });
    }

    @Override
    public void close() {
        if (client != null && isStarted.get()) {
            client.shutdown();
            isStarted.set(false);
        }
    }
}
