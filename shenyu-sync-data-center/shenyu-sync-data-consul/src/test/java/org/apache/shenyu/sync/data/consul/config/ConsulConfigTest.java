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

package org.apache.shenyu.sync.data.consul.config;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * add test case for {@link ConsulConfig}.
 */
public class ConsulConfigTest {

    private static final String URL = "url";

    private static final int WAIT_TIME = 1000;

    private static final int WATCH_DELAY = 100;

    private ConsulConfig consulConfig;

    private ConsulConfig that;

    @BeforeEach
    public void setUp() {
        consulConfig = new ConsulConfig();
        consulConfig.setUrl(URL);
        consulConfig.setWaitTime(WAIT_TIME);
        consulConfig.setWatchDelay(WATCH_DELAY);
        that = new ConsulConfig();
        that.setUrl(URL);
        that.setWaitTime(WAIT_TIME);
        that.setWatchDelay(WATCH_DELAY);
    }

    @Test
    public void testGetterSetter() {
        assertEquals(URL, consulConfig.getUrl());
        assertEquals(WAIT_TIME, consulConfig.getWaitTime());
        assertEquals(WATCH_DELAY, consulConfig.getWatchDelay());
    }

    @Test
    public void testEquals() {
        assertEquals(consulConfig, consulConfig);
        assertEquals(consulConfig, that);
        assertNotEquals(consulConfig, null);
        assertNotEquals(consulConfig, new Object());
    }

    @Test
    public void testHashCode() {
        assertEquals(Objects.hash(consulConfig.getUrl(), consulConfig.getWaitTime(),
                        consulConfig.getWatchDelay()),
                consulConfig.hashCode());
    }

    @Test
    public void testToString() {
        assertNotNull(consulConfig.toString());
    }
}
