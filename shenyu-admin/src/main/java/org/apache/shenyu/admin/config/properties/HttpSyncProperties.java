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

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;

import java.time.Duration;

/**
 * the http sync strategy properties.
 */
@Getter
@Setter
@ConfigurationProperties(prefix = "shenyu.sync.http")
public class HttpSyncProperties {

    /**
     * Whether enabled http sync strategy, default: true.
     */
    private boolean enabled = true;

    /**
     * Periodically refresh the config data interval from the database, default: 5 minutes.
     */
    private Duration refreshInterval = Duration.ofMinutes(5);

    /**
     * Notify client batch size.
     */
    private int notifyBatchSize = 100;

}
