/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.limiter.redis.config;

import lombok.Data;
import org.dromara.soul.config.api.ConfigParent;

/**
 * The RedisConfig.
 *
 * @author xiaoyu
 */
@Data
public class RedisConfig extends ConfigParent {

    private Boolean cluster = false;

    private Boolean sentinel = false;

    /**
     * cluster url example:ip:port;ip:port.
     */
    private String clusterUrl;

    /**
     * sentinel url example:ip:port;ip:port.
     */
    private String sentinelUrl;

    private String masterName;

    private String host;

    private int port;

    private String password;

    private int maxTotal = 8;

    private int maxIdle = 8;

    private int minIdle;

    private long maxWaitMillis = -1L;

    private long minEvictableIdleTimeMillis = 1800000L;

    private long softMinEvictableIdleTimeMillis = 1800000L;

    private int numTestsPerEvictionRun = 3;

    private Boolean testOnCreate = false;

    private Boolean testOnBorrow = false;

    private Boolean testOnReturn = false;

    private Boolean testWhileIdle = false;

    private long timeBetweenEvictionRunsMillis = -1L;

    private boolean blockWhenExhausted = true;

    private int timeOut = 10000;

    @Override
    public String prefix() {
        return "soul.redis";
    }
}
