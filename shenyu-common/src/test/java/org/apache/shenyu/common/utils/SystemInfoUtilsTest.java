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

package org.apache.shenyu.common.utils;

import org.junit.jupiter.api.Test;

import java.lang.management.ManagementFactory;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for SystemInfoUtils.
 */
public final class SystemInfoUtilsTest {

    @Test
    public void testGetSystemInfoContainsBootTime() {
        String systemInfo = SystemInfoUtils.getSystemInfo();
        Map<String, Object> infoMap = GsonUtils.getInstance().convertToMap(systemInfo);
        assertNotNull(infoMap.get("arch"));
        assertNotNull(infoMap.get("operatingSystem"));
        // boot identity used to correlate instance status with metrics, logs and traces
        Object bootTime = infoMap.get("bootTime");
        assertNotNull(bootTime);
        assertTrue(bootTime instanceof Number);
        assertEquals(ManagementFactory.getRuntimeMXBean().getStartTime(), ((Number) bootTime).longValue());
    }
}
