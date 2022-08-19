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

package org.apache.shenyu.plugin.tencent.cls.config;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test cases for LogCollectConfig.
 */
public class TencentLogCollectConfigTest {

    private final TencentLogCollectConfig.TencentClsLogConfig tencentClsLogConfig = new TencentLogCollectConfig.TencentClsLogConfig();

    @Test
    public void testLogCollectorConfigHost() {
        tencentClsLogConfig.setSecretId("secretId");
        tencentClsLogConfig.setSecretKey("secretKey");
        tencentClsLogConfig.setEndpoint("endpoint");
        tencentClsLogConfig.setTopic("topic");
        tencentClsLogConfig.setTotalSizeInBytes("totalSizeInBytes");
        tencentClsLogConfig.setMaxSendThreadCount("maxSendThreadCount");
        tencentClsLogConfig.setMaxBlockSec("maxBlockSec");
        tencentClsLogConfig.setMaxBatchSize("maxBatchSize");
        tencentClsLogConfig.setMaxBatchCount("maxBatchCount");
        tencentClsLogConfig.setLingerMs("lingerMs");
        tencentClsLogConfig.setRetries("retries");
        tencentClsLogConfig.setMaxReservedAttempts("maxReservedAttempts");
        tencentClsLogConfig.setBaseRetryBackoffMs("baseRetryBackoffMs");
        tencentClsLogConfig.setMaxRetryBackoffMs("maxRetryBackoffMs");
        tencentClsLogConfig.setSendThreadCount(1);
        Assertions.assertEquals(tencentClsLogConfig.getSecretId(), "secretId");
        Assertions.assertEquals(tencentClsLogConfig.getSecretKey(), "secretKey");
        Assertions.assertEquals(tencentClsLogConfig.getEndpoint(), "endpoint");
        Assertions.assertEquals(tencentClsLogConfig.getTopic(), "topic");
        Assertions.assertEquals(tencentClsLogConfig.getTotalSizeInBytes(), "totalSizeInBytes");
        Assertions.assertEquals(tencentClsLogConfig.getMaxSendThreadCount(), "maxSendThreadCount");
        Assertions.assertEquals(tencentClsLogConfig.getMaxBlockSec(), "maxBlockSec");
        Assertions.assertEquals(tencentClsLogConfig.getMaxBatchSize(), "maxBatchSize");
        Assertions.assertEquals(tencentClsLogConfig.getMaxBatchCount(), "maxBatchCount");
        Assertions.assertEquals(tencentClsLogConfig.getLingerMs(), "lingerMs");
        Assertions.assertEquals(tencentClsLogConfig.getRetries(), "retries");
        Assertions.assertEquals(tencentClsLogConfig.getMaxReservedAttempts(), "maxReservedAttempts");
        Assertions.assertEquals(tencentClsLogConfig.getBaseRetryBackoffMs(), "baseRetryBackoffMs");
        Assertions.assertEquals(tencentClsLogConfig.getMaxRetryBackoffMs(), "maxRetryBackoffMs");
        Assertions.assertEquals(tencentClsLogConfig.getSendThreadCount(), 1);
    }

}
