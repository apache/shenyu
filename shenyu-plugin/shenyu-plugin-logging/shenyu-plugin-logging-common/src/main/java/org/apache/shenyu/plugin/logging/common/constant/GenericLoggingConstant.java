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

package org.apache.shenyu.plugin.logging.common.constant;

/**
 * generic logging constant.
 */
public class GenericLoggingConstant {
    
    /**
     * logging user agent.
     */
    public static final String USER_AGENT = "User-Agent";
    
    /**
     * logging user host.
     */
    public static final String HOST = "Host";
    
    /**
     * shenyu agent trace id.
     */
    public static final String SHENYU_AGENT_TRACE_ID = "shenyu-agent-trace-id";
    
    /**
     * aliyun sls accessId.
     */
    public static final String ACCESS_ID = "AccessId";
    
    /**
     * aliyun sls accessKey.
     */
    public static final String ACCESS_KEY = "AccessKey";
    
    /**
     * aliyun sls project.
     */
    public static final String PROJECT_NAME = "ProjectName";
    
    /**
     * aliyun sls logstore name.
     */
    public static final String LOG_STORE = "LogStore";
    
    /**
     * aliyun sls ttl in day.
     */
    public static final String TTL_IN_DAY = "TtlInDay";
    
    /**
     * aliyun sls shard count.
     */
    public static final String SHARD_COUNT = "ShardCount";
    
    /**
     * aliyun sls topic.
     */
    public static final String TOPIC = "topic";

    /**
     * tencent cls secretId.
     */
    public static final String SECRET_ID = "secretId";

    /**
     * tencent cls secretKey.
     */
    public static final String SECRET_KEY = "secretKey";

    /**
     * tencent cls endpoint.
     */
    public static final String ENDPOINT = "endpoint";

    /**
     * tencent cls totalSizeInBytes.
     */
    public static final String TOTAL_SIZE_IN_BYTES = "totalSizeInBytes";

    /**
     * tencent cls maxSendThreadCount.
     */
    public static final String MAX_SEND_THREAD_COUNT = "maxSendThreadCount";

    /**
     * tencent cls maxBlockSec.
     */
    public static final String MAX_BLOCK_SEC = "maxBlockSec";

    /**
     * tencent cls maxBatchSize.
     */
    public static final String MAX_BATCH_SIZE = "maxBatchSize";

    /**
     * tencent cls maxBatchCount.
     */
    public static final String MAX_BATCH_COUNT = "maxBatchCount";

    /**
     * tencent cls lingerMs.
     */
    public static final String LINGER_MS = "lingerMs";

    /**
     * tencent cls retries.
     */
    public static final String RETRIES = "retries";

    /**
     * tencent cls maxReservedAttempts.
     */
    public static final String MAX_RESERVED_ATTEMPTS = "maxReservedAttempts";

    /**
     * tencent cls baseRetryBackoffMs.
     */
    public static final String BASE_RETRY_BACKOFF_MS = "baseRetryBackoffMs";

    /**
     * tencent cls maxRetryBackoffMs.
     */
    public static final String MAX_RETRY_BACKOFF_MS = "maxRetryBackoffMs";

    /**
     * send thread config.
     */
    public static final String SEND_THREAD_COUNT = "sendThreadCount";
    
    /**
     * io thread count.
     */
    public static final String IO_THREAD_COUNT = "ioThreadCount";
    
    /**
     * system default, max threads.
     */
    public static final Integer MAX_ALLOW_THREADS = 500;
    
    /**
     * max queue.
     */
    public static final Integer MAX_QUEUE_NUMBER = 10000;
    
    /**
     * default source.
     */
    public static final String DEFAULT_SOURCE = "shenyu-gateway";
    
    /**
     * The constant INDEX.
     */
    public static final String INDEX = "shenyu-access-logging";
    
    /**
     * The constant PORT.
     */
    public static final String PORT = "port";
    
    /**
     * The constant NAMESERVER_ADDRESS.
     */
    public static final String NAMESERVER_ADDRESS = "namesrvAddr";

    /**
     * The constant SERVICE_URL.
     */
    public static final String SERVICE_URL = "serviceUrl";
    
    /**
     * The constant PRODUCER_GROUP.
     */
    public static final String PRODUCER_GROUP = "producerGroup";
}
