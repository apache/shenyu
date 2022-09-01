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

package org.apache.shenyu.plugin.logging.pulsar.config;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class PulsarCollectConfigTest {

    @Test
    public void testSetLogApiConfigTopic() {
        PulsarLogCollectConfig.LogApiConfig logApiConfig = new PulsarLogCollectConfig.LogApiConfig();
        logApiConfig.setTopic("test");
        Assertions.assertEquals(logApiConfig.getTopic(), "test");
    }

    @Test
    public void testSetLogApiConfigSampleRate() {
        PulsarLogCollectConfig.LogApiConfig logApiConfig = new PulsarLogCollectConfig.LogApiConfig();
        logApiConfig.setSampleRate("1");
        Assertions.assertEquals(logApiConfig.getSampleRate(), "1");
    }

    @Test
    public void testGetGlobalLogConfigSampleRate() {
        PulsarLogCollectConfig.PulsarLogConfig pulsarLogConfig = new PulsarLogCollectConfig.PulsarLogConfig();
        pulsarLogConfig.setSampleRate("1");
        Assertions.assertEquals(pulsarLogConfig.getSampleRate(), "1");
    }

    @Test
    public void testSetGlobalLogConfigTopic() {
        PulsarLogCollectConfig.PulsarLogConfig pulsarLogConfig = new PulsarLogCollectConfig.PulsarLogConfig();
        pulsarLogConfig.setTopic("test");
        Assertions.assertEquals(pulsarLogConfig.getTopic(), "test");
    }

    @Test
    public void testSetGlobalLogConfigMaxResponseBody() {
        PulsarLogCollectConfig.PulsarLogConfig pulsarLogConfig = new PulsarLogCollectConfig.PulsarLogConfig();
        pulsarLogConfig.setMaxResponseBody(5);
        Assertions.assertEquals(pulsarLogConfig.getMaxResponseBody(), 5);
    }

    @Test
    public void testSetGlobalLogConfigMaxRequestBody() {
        PulsarLogCollectConfig.PulsarLogConfig pulsarLogConfig = new PulsarLogCollectConfig.PulsarLogConfig();
        pulsarLogConfig.setMaxRequestBody(5);
        Assertions.assertEquals(pulsarLogConfig.getMaxRequestBody(), 5);
    }

    @Test
    public void testSetGlobalLogConfigServiceUrl() {
        PulsarLogCollectConfig.PulsarLogConfig pulsarLogConfig = new PulsarLogCollectConfig.PulsarLogConfig();
        pulsarLogConfig.setServiceUrl("test");
        Assertions.assertEquals(pulsarLogConfig.getServiceUrl(), "test");
    }

    @Test
    public void testGetGlobalLogConfig() {
        PulsarLogCollectConfig pulsarLogCollectConfig = new PulsarLogCollectConfig();
        PulsarLogCollectConfig.PulsarLogConfig pulsarLogConfig = new PulsarLogCollectConfig.PulsarLogConfig();
        pulsarLogCollectConfig.setPulsarLogConfig(pulsarLogConfig);
        Assertions.assertEquals(pulsarLogCollectConfig.getPulsarLogConfig(), pulsarLogConfig);
    }

    @Test
    public void testCompressAlg() {
        PulsarLogCollectConfig.PulsarLogConfig pulsarLogConfig = new PulsarLogCollectConfig.PulsarLogConfig();
        pulsarLogConfig.setCompressAlg("LZ4");
        Assertions.assertEquals(pulsarLogConfig.getCompressAlg(), "LZ4");
    }

    @Test
    public void testBufferQueueSize() {
        PulsarLogCollectConfig.PulsarLogConfig pulsarLogConfig = new PulsarLogCollectConfig.PulsarLogConfig();
        pulsarLogConfig.setBufferQueueSize(50000);
        Assertions.assertEquals(pulsarLogConfig.getBufferQueueSize(), 50000);
    }
}
