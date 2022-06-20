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

package org.apache.shenyu.plugin.logging.elasticsearch.config;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * The Test Case For LogCollectConfig.
 */
public final class LogCollectConfigTest {

    private LogCollectConfig logCollectConfig = new LogCollectConfig();

    @Test
    public void testSetLogApiConfigSampleRate() {
        LogCollectConfig.LogApiConfig logApiConfig = new LogCollectConfig.LogApiConfig();
        logApiConfig.setSampleRate("1");
        Assertions.assertEquals(logApiConfig.getSampleRate(), "1");
    }

    @Test
    public void testGetGlobalLogConfigSampleRate() {
        LogCollectConfig.GlobalLogConfig globalLogConfig = new LogCollectConfig.GlobalLogConfig();
        globalLogConfig.setSampleRate("1");
        Assertions.assertEquals(globalLogConfig.getSampleRate(), "1");
    }

    @Test
    public void testSetGlobalLogConfigMaxResponseBody() {
        LogCollectConfig.GlobalLogConfig globalLogConfig = new LogCollectConfig.GlobalLogConfig();
        globalLogConfig.setMaxResponseBody(5);
        Assertions.assertEquals(globalLogConfig.getMaxResponseBody(), 5);
    }

    @Test
    public void testSetGlobalLogConfigMaxRequestBody() {
        LogCollectConfig.GlobalLogConfig globalLogConfig = new LogCollectConfig.GlobalLogConfig();
        globalLogConfig.setMaxRequestBody(5);
        Assertions.assertEquals(globalLogConfig.getMaxRequestBody(), 5);
    }

    @Test
    public void testSetGlobalLogConfigHost() {
        LogCollectConfig.GlobalLogConfig globalLogConfig = new LogCollectConfig.GlobalLogConfig();
        globalLogConfig.setHost("localhost");
        Assertions.assertEquals(globalLogConfig.getHost(), "localhost");
    }

    @Test
    public void testSetGlobalLogConfigPort() {
        LogCollectConfig.GlobalLogConfig globalLogConfig = new LogCollectConfig.GlobalLogConfig();
        globalLogConfig.setPort("9200");
        Assertions.assertEquals(globalLogConfig.getPort(), "9200");
    }

    @Test
    public void testGetGlobalLogConfig() {
        LogCollectConfig.GlobalLogConfig globalLogConfig = new LogCollectConfig.GlobalLogConfig();
        logCollectConfig.setGlobalLogConfig(globalLogConfig);
        Assertions.assertEquals(logCollectConfig.getGlobalLogConfig(), globalLogConfig);
    }
}
