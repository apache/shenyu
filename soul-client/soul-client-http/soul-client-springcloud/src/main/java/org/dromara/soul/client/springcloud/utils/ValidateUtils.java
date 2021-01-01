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

package org.dromara.soul.client.springcloud.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.client.springcloud.config.SoulSpringCloudConfig;
import org.springframework.core.env.Environment;

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
     * validate SoulSpringCloudConfig.
     *
     * @param soulSpringCloudConfig the soulSpringCloudConfig
     * @param env                   the env
     * @throws RuntimeException the RuntimeException
     */
    public static void validate(final SoulSpringCloudConfig soulSpringCloudConfig, final Environment env) {
        String contextPath = soulSpringCloudConfig.getContextPath();
        String adminUrl = soulSpringCloudConfig.getAdminUrl();
        String appName = env.getProperty("spring.application.name");
        if (StringUtils.isNotBlank(contextPath) && StringUtils.isNotBlank(adminUrl) && StringUtils.isNotBlank(appName)) {
            return;
        }
        String errorMsg = "spring cloud param must config the contextPath, adminUrl and appName";
        log.error(errorMsg);
        throw new RuntimeException(errorMsg);
    }

}
