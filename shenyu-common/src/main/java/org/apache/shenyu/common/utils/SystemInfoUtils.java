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

import com.sun.management.OperatingSystemMXBean;
import oshi.SystemInfo;

import java.lang.management.ManagementFactory;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Map;

/**
 * The type System info utils.
 */
public final class SystemInfoUtils {
    
    /**
     * Gets system info.
     *
     * @return the system info
     */
    public static String getSystemInfo() {
        try {
            // Get host information using OSHI
            SystemInfo systemInfo = new SystemInfo();
            
            // Get host information
            OperatingSystemMXBean osBean =
                    (OperatingSystemMXBean) ManagementFactory.getOperatingSystemMXBean();
            Map<String, Object> hostInfo = Map.of(
                    "arch", osBean.getArch(),
                    "operatingSystem", systemInfo.getOperatingSystem().toString(),
                    "availableProcessors", osBean.getAvailableProcessors(),
                    "totalMemorySizeGB", bytesToGB(osBean.getTotalMemorySize()) + " GB"
            );
            return GsonUtils.getInstance().toJson(hostInfo);
        } catch (Exception e) {
            // Handle any exceptions that may occur
            return "Error retrieving system information: " + e.getMessage();
        }
    }
    
    /**
     * Bytes to gb double.
     *
     * @param bytesValue the bytes value
     * @return the double
     */
    private static double bytesToGB(final long bytesValue) {
        return BigDecimal.valueOf(bytesValue / (1024.0 * 1024 * 1024)).setScale(2, RoundingMode.HALF_UP).doubleValue();
    }
}
