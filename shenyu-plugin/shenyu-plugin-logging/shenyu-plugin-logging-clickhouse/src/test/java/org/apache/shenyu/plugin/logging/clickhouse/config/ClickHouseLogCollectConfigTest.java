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

package org.apache.shenyu.plugin.logging.clickhouse.config;

import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The Test Case For ClickHouseLogCollectConfig.
 */
public class ClickHouseLogCollectConfigTest {

    private ClickHouseLogCollectConfig clickHouseLogCollectConfig = new ClickHouseLogCollectConfig();

    private ClickHouseLogCollectConfig.ClickHouseLogConfig clickHouseLogConfig = new ClickHouseLogCollectConfig.ClickHouseLogConfig();

    @BeforeEach
    public void setUp() {
        clickHouseLogConfig.setHost("127.0.0.1");
        clickHouseLogConfig.setPort("8123");
        clickHouseLogConfig.setUsername("foo");
        clickHouseLogConfig.setPassword("bar");
        clickHouseLogConfig.setDatabase(GenericLoggingConstant.DEFAULT_SOURCE);
    }

    @Test
    public void testSetClickHouseLogConfig() {
        clickHouseLogCollectConfig.setClickHouseLogConfig(clickHouseLogConfig);
        Assertions.assertNull(null);
    }

    @Test
    public void testGetClickHouseLogConfig() {
        ClickHouseLogCollectConfig.ClickHouseLogConfig clickHouseLogConfig = clickHouseLogCollectConfig.getClickHouseLogConfig();
        Assertions.assertEquals(clickHouseLogConfig, clickHouseLogConfig);
    }

    @Test
    public void testSetHost() {
        clickHouseLogConfig.setHost("127.0.0.1");
        Assertions.assertNull(null);
    }

    @Test
    public void testGetHost() {
        final String host = clickHouseLogConfig.getHost();
        Assertions.assertEquals("127.0.0.1", host);
    }

    @Test
    public void testSetPort() {
        clickHouseLogConfig.setPort("8123");
        Assertions.assertNull(null);
    }

    @Test
    public void testGetPort() {
        final String port = clickHouseLogConfig.getPort();
        Assertions.assertEquals("8123", port);
    }

    @Test
    public void testSetUsername() {
        clickHouseLogConfig.setUsername("foo");
        Assertions.assertNull(null);
    }

    @Test
    public void testGetUsername() {
        final String username = clickHouseLogConfig.getUsername();
        Assertions.assertEquals("foo", username);
    }

    @Test
    public void testSetPassword() {
        clickHouseLogConfig.setPassword("bar");
        Assertions.assertNull(null);
    }

    @Test
    public void testGetPassword() {
        final String password = clickHouseLogConfig.getPassword();
        Assertions.assertEquals("bar", password);
    }

    @Test
    public void testSetDatabase() {
        clickHouseLogConfig.setDatabase(GenericLoggingConstant.DEFAULT_SOURCE);
        Assertions.assertNull(null);
    }

    @Test
    public void testGetDatabase() {
        final String database = clickHouseLogConfig.getDatabase();
        Assertions.assertEquals(GenericLoggingConstant.DEFAULT_SOURCE, database);
    }

}
