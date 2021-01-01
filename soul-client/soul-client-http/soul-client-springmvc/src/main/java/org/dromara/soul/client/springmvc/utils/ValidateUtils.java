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

package org.dromara.soul.client.springmvc.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;

/**
 * ValidateUtils.
 *
 * @author severez
 */
@Slf4j
public final class ValidateUtils {
    private ValidateUtils() {
    }

    /**
     * validate SoulSpringMvcConfig.
     *
     * @param soulSpringMvcConfig the soulSpringMvcConfig
     * @throws RuntimeException the RuntimeException
     */
    public static void validate(final SoulSpringMvcConfig soulSpringMvcConfig) {
        String contextPath = soulSpringMvcConfig.getContextPath();
        String adminUrl = soulSpringMvcConfig.getAdminUrl();
        Integer port = soulSpringMvcConfig.getPort();
        if (StringUtils.isNotBlank(contextPath) && StringUtils.isNotBlank(adminUrl) && port != null) {
            return;
        }
        String errorMsg = "spring mvc param must config contextPath, adminUrl and port";
        log.error(errorMsg);
        throw new RuntimeException(errorMsg);
    }

}
