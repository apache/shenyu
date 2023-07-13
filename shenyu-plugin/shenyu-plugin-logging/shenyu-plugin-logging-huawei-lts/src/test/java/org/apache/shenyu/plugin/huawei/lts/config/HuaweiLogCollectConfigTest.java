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

package org.apache.shenyu.plugin.huawei.lts.config;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class HuaweiLogCollectConfigTest {
    private final HuaweiLogCollectConfig.HuaweiLtsLogConfig huaweiLtsLogConfig = new HuaweiLogCollectConfig.HuaweiLtsLogConfig();

    @Test
    public void testLogCollectorConfigHost() {
        huaweiLtsLogConfig.setAccessKeyId("AccessKeyId");
        huaweiLtsLogConfig.setAccessKeySecret("AccessKeySecret");
        huaweiLtsLogConfig.setRegionName("RegionName");
        huaweiLtsLogConfig.setLogGroupId("LogGroupId");
        huaweiLtsLogConfig.setLogStreamId("LogStreamId");
        huaweiLtsLogConfig.setProjectId("ProjectId");
        huaweiLtsLogConfig.setTotalSizeInBytes(104857600);
        huaweiLtsLogConfig.setMaxBlockMs(0L);
        huaweiLtsLogConfig.setIoThreadCount(8);
        huaweiLtsLogConfig.setBatchSizeThresholdInBytes(524288);
        huaweiLtsLogConfig.setBatchCountThreshold(4096);
        huaweiLtsLogConfig.setLingerMs(2000);
        huaweiLtsLogConfig.setRetries(1);
        huaweiLtsLogConfig.setBaseRetryBackoffMs(100L);
        huaweiLtsLogConfig.setMaxRetryBackoffMs(100L);
        Assertions.assertEquals(huaweiLtsLogConfig.getAccessKeyId(), "AccessKeyId");
        Assertions.assertEquals(huaweiLtsLogConfig.getAccessKeySecret(), "AccessKeySecret");
        Assertions.assertEquals(huaweiLtsLogConfig.getRegionName(), "RegionName");
        Assertions.assertEquals(huaweiLtsLogConfig.getLogGroupId(), "LogGroupId");
        Assertions.assertEquals(huaweiLtsLogConfig.getLogStreamId(), "LogStreamId");
        Assertions.assertEquals(huaweiLtsLogConfig.getProjectId(), "ProjectId");
        Assertions.assertEquals(huaweiLtsLogConfig.getTotalSizeInBytes(), 104857600);
        Assertions.assertEquals(huaweiLtsLogConfig.getMaxBlockMs(), 0L);
        Assertions.assertEquals(huaweiLtsLogConfig.getIoThreadCount(), 8);
        Assertions.assertEquals(huaweiLtsLogConfig.getBatchSizeThresholdInBytes(), 524288);
        Assertions.assertEquals(huaweiLtsLogConfig.getBatchCountThreshold(), 4096);
        Assertions.assertEquals(huaweiLtsLogConfig.getLingerMs(), 2000);
        Assertions.assertEquals(huaweiLtsLogConfig.getRetries(), 1);
        Assertions.assertEquals(huaweiLtsLogConfig.getBaseRetryBackoffMs(), 100L);
        Assertions.assertEquals(huaweiLtsLogConfig.getMaxRetryBackoffMs(), 100L);
    }
}
