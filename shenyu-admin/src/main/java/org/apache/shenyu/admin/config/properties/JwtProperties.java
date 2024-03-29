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

package org.apache.shenyu.admin.config.properties;

import org.apache.shenyu.common.constant.AdminConstants;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * Jwt Properties.
 */
@Component
@ConfigurationProperties(prefix = "shenyu.jwt")
public class JwtProperties {

    private Long expiredSeconds = AdminConstants.THE_ONE_DAY_MILLIS_TIME;

    /**
     * Gets the value of expiredSeconds.
     *
     * @return the value of expiredSeconds
     */
    public Long getExpiredSeconds() {
        return expiredSeconds;
    }

    /**
     * Sets the expiredSeconds.
     *
     * @param expiredSeconds expiredSeconds
     */
    public void setExpiredSeconds(final Long expiredSeconds) {
        this.expiredSeconds = expiredSeconds;
    }
}
