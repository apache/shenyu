/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.dromara.soul.remoting.redis;

import java.util.Set;
import lombok.Data;

/**
 * RedisConfig .
 * redis config setting info.
 *
 * @author sixh
 */
@Data
public final class RedisModule {

    private String mode;

    private Set<String> hosts;

    private String masterName;

    private String password;

    private Integer maxTotal = 8;

    private Integer maxIdle = 8;

    private Integer minIdle;

    private Long maxWaitMillis = -1L;

    private Long minEvictableIdleTimeMillis = 1800000L;

    private Long softMinEvictableIdleTimeMillis = 1800000L;

    private Integer numTestsPerEvictionRun = 3;

    private Boolean testOnCreate = false;

    private Boolean testOnBorrow = false;

    private Boolean testOnReturn = false;

    private Boolean testWhileIdle = false;

    private Long timeBetweenEvictionRunsMillis = -1L;

    private Boolean blockWhenExhausted = true;

    private Integer timeOut = 10000;
}
