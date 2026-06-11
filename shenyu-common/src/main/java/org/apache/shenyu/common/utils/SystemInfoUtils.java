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
import org.apache.shenyu.common.exception.ShenyuException;
import oshi.SystemInfo;

import java.lang.management.ManagementFactory;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Map;

import static org.apache.shenyu.common.constant.Constants.ARCH;
import static org.apache.shenyu.common.constant.Constants.AVAILABLE_PROCESSORS;
import static org.apache.shenyu.common.constant.Constants.GB;
import static org.apache.shenyu.common.constant.Constants.OPERATING_SYSTEM;
import static org.apache.shenyu.common.constant.Constants.TOTAL_MEMORY_SIZE_GB;

/**
 * The type System info utils.
 */
public final class SystemInfoUtils {

    private static final int BYTES_IN_KB = 1024;

    private static final int BYTES_IN_MB = BYTES_IN_KB * 1024;

    private static final int BYTES_IN_GB = BYTES_IN_MB * 1024;

    private static final int DECIMAL_PLACES = 2;

    private static final RoundingMode ROUNDING_MODE = RoundingMode.HALF_UP;

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
                    ARCH, osBean.getArch(),
                    OPERATING_SYSTEM, systemInfo.getOperatingSystem().toString(),
                    AVAILABLE_PROCESSORS, osBean.getAvailableProcessors(),
                    TOTAL_MEMORY_SIZE_GB, bytesToGB(osBean.getTotalMemorySize()) + GB
            );
            return GsonUtils.getInstance().toJson(hostInfo);
        } catch (Exception e) {
            // Handle any exceptions that may occur
            throw new ShenyuException("Error retrieving system information: " + e.getMessage());
        }
    }

    /**
     * Bytes to gb double.
     *
     * @param bytesValue the bytes value
     * @return the double
     */
    private static double bytesToGB(final long bytesValue) {
        return BigDecimal.valueOf(bytesValue / (double) BYTES_IN_GB)
                .setScale(DECIMAL_PLACES, ROUNDING_MODE)
                .doubleValue();
    }
}
