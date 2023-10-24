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

package org.apache.shenyu.admin.model.bean;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * test cast for {@link UpstreamInstance}.
 */
public class UpstreamInstanceTest {

    private UpstreamInstance upstreamInstance;

    @BeforeEach
    public void setup() {
        upstreamInstance = new UpstreamInstance();
        upstreamInstance.setContextPath("ShenyuContextPath");
        upstreamInstance.setIp("0.0.0.0");
        upstreamInstance.setPort(9195);
        upstreamInstance.setStartupTime(123L);
        upstreamInstance.setEnabled(true);
        upstreamInstance.setHealthy(true);
    }

    @Test
    public void testEquals() {
        assertEquals("ShenyuContextPath", upstreamInstance.getContextPath());
        assertEquals("0.0.0.0", upstreamInstance.getIp());
        assertEquals(9195, upstreamInstance.getPort());
        assertEquals(123L, upstreamInstance.getStartupTime());
        assertTrue(upstreamInstance.isEnabled());
        assertTrue(upstreamInstance.isHealthy());
    }

    @Test
    public void testGetClusterName() {
        assertEquals(upstreamInstance.getClusterName(), "henyuContextPath");
        upstreamInstance.setContextPath("");
        assertNull(upstreamInstance.getClusterName());
    }

    @Test
    public void testHashCode() {
        assertEquals(236671917, upstreamInstance.hashCode());
    }
}
