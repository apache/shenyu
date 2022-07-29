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

import org.apache.shenyu.plugin.logging.elasticsearch.config.ElasticSearchLogCollectConfig.LogApiConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * The Test Case For LogCollectConfig.
 */
public final class ElasticSearchLogCollectConfigTest {

    @Test
    public void testSetLogApiConfigSampleRate() {
        ElasticSearchLogCollectConfig.LogApiConfig logApiConfig = new ElasticSearchLogCollectConfig.LogApiConfig();
        logApiConfig.setSampleRate("1");
        Assertions.assertEquals(logApiConfig.getSampleRate(), "1");
    }

    @Test
    public void testGetGlobalLogConfigSampleRate() {
        ElasticSearchLogCollectConfig.ElasticSearchLogConfig elasticSearchLogConfig = new ElasticSearchLogCollectConfig.ElasticSearchLogConfig();
        elasticSearchLogConfig.setSampleRate("1");
        Assertions.assertEquals(elasticSearchLogConfig.getSampleRate(), "1");
    }

    @Test
    public void testSetGlobalLogConfigMaxResponseBody() {
        ElasticSearchLogCollectConfig.ElasticSearchLogConfig elasticSearchLogConfig = new ElasticSearchLogCollectConfig.ElasticSearchLogConfig();
        elasticSearchLogConfig.setMaxResponseBody(5);
        Assertions.assertEquals(elasticSearchLogConfig.getMaxResponseBody(), 5);
    }

    @Test
    public void testSetGlobalLogConfigMaxRequestBody() {
        ElasticSearchLogCollectConfig.ElasticSearchLogConfig elasticSearchLogConfig = new ElasticSearchLogCollectConfig.ElasticSearchLogConfig();
        elasticSearchLogConfig.setMaxRequestBody(5);
        Assertions.assertEquals(elasticSearchLogConfig.getMaxRequestBody(), 5);
    }

    @Test
    public void testSetGlobalLogConfigHost() {
        ElasticSearchLogCollectConfig.ElasticSearchLogConfig elasticSearchLogConfig = new ElasticSearchLogCollectConfig.ElasticSearchLogConfig();
        elasticSearchLogConfig.setHost("localhost");
        Assertions.assertEquals(elasticSearchLogConfig.getHost(), "localhost");
    }

    @Test
    public void testSetGlobalLogConfigPort() {
        ElasticSearchLogCollectConfig.ElasticSearchLogConfig elasticSearchLogConfig = new ElasticSearchLogCollectConfig.ElasticSearchLogConfig();
        elasticSearchLogConfig.setPort("9200");
        Assertions.assertEquals(elasticSearchLogConfig.getPort(), "9200");
    }

    @Test
    public void testGetGlobalLogConfig() {
        ElasticSearchLogCollectConfig elasticSearchLogCollectConfig = new ElasticSearchLogCollectConfig();
        ElasticSearchLogCollectConfig.ElasticSearchLogConfig elasticSearchLogConfig = new ElasticSearchLogCollectConfig.ElasticSearchLogConfig();
        elasticSearchLogCollectConfig.setElasticSearchLogConfig(elasticSearchLogConfig);
        Assertions.assertEquals(elasticSearchLogCollectConfig.getElasticSearchLogConfig(), elasticSearchLogConfig);
    }

    @Test
    public void testCompressAlg() {
        ElasticSearchLogCollectConfig.ElasticSearchLogConfig elasticSearchLogConfig = new ElasticSearchLogCollectConfig.ElasticSearchLogConfig();
        elasticSearchLogConfig.setCompressAlg("LZ4");
        Assertions.assertEquals(elasticSearchLogConfig.getCompressAlg(), "LZ4");
    }

    @Test
    public void testIndex() {
        ElasticSearchLogCollectConfig.LogApiConfig logApiConfig = new LogApiConfig();
        logApiConfig.setIndex("shenyu-access-logging");
        Assertions.assertEquals(logApiConfig.getIndex(), "shenyu-access-logging");
    }
}
