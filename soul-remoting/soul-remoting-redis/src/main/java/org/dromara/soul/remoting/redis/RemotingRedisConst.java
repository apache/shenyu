/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.remoting.redis;

/**
 * RemotingRedisConst
 * const.
 *
 * @author sixh
 */
public interface RemotingRedisConst {

    String URL_CLIENT_KEY = "client";

    /**
     * The constant URL_CLUSTER_KEY.
     */
    String URL_CLUSTER_KEY = "cluster";

    /**
     * The constant URL_MASTER_NAME_KEY.
     */
    String URL_MASTER_NAME_KEY = "masterName";

    /**
     * The constant URL_MODE_KEY.
     */
    String URL_MODE_KEY = "mode";

    /**
     * The constant URL_PASSWORD_KEY.
     */
    String URL_PASSWORD_KEY = "password";
    /**
     * The constant URL_MAX_TOTAL_KEY.
     */
    String URL_MAX_TOTAL_KEY = "maxTotal";

    Integer MAX_TOTAL_DEFAULT = 8;

    /**
     * The constant URL_MAX_IDLE_KEY.
     */
    String URL_MAX_IDLE_KEY = "maxIdle";

    Integer MAX_IDLE_DEFAULT = 8;

    /**
     * The constant URL_MIN_IDLE_KEY.
     */
    String URL_MIN_IDLE_KEY = "minIdle";

    /**
     * The constant URL_MAX_WAIT_MILLIS_KEY.
     */
    String URL_MAX_WAIT_MILLIS_KEY = "maxWaitMillis";

    Long MAX_WAIT_MILLIS_DEFAULT = -1L;

    /**
     * The constant URL_MIN_EVICTABLE_IDLE_TIME_MILLIS_KEY.
     */
    String URL_MIN_EVICTABLE_IDLE_TIME_MILLIS_KEY = "minEvictableIdleTimeMillis";

    Long MIN_EVICTABLE_IDLE_TIME_MILLIS_DEFAULT = 1800000L;
    /**
     * The constant URL_SOFT_MIN_EVICTABLE_IDLE_TIME_MILLIS_KEY.
     */
    String URL_SOFT_MIN_EVICTABLE_IDLE_TIME_MILLIS_KEY = "softMinEvictableIdleTimeMillis";

    Long SOFT_MIN_EVICTABLE_IDLE_TIME_MILLIS_DEFAULT = 1800000L;

    /**
     * The constant URL_NUM_TESTS_PER_EVICTION_RUN_KEY.
     */
    String URL_NUM_TESTS_PER_EVICTION_RUN_KEY = "numTestsPerEvictionRun";

    Integer NUM_TESTS_PER_EVICTION_RUN_DEFAULT = 3;

    /**
     * The constant URL_TEST_ON_CREATE_KEY.
     */
    String URL_TEST_ON_CREATE_KEY = "testOnCreate";

    Boolean TEST_ON_CREATE_DEFAULT = false;

    /**
     * The constant URL_TEST_ON_BORROW_KEY.
     */
    String URL_TEST_ON_BORROW_KEY = "testOnBorrow";

    Boolean TEST_ON_BORROW_DEFAULT = false;

    /**
     * The constant URL_TEST_ON_RETURN_KEY.
     */
    String URL_TEST_ON_RETURN_KEY = "testOnReturn";

    Boolean TEST_ON_RETURN_DEFAULT = false;

    /**
     * The constant URL_TEST_WHILE_IDLE_KEY.
     */
    String URL_TEST_WHILE_IDLE_KEY = "testWhileIdle";

    Boolean TEST_WHILE_IDLE_DEFAULT = false;

    /**
     * The constant URL_TIME_BETWEEN_EVICTION_RUNS_MILLIS_KEY.
     */
    String URL_TIME_BETWEEN_EVICTION_RUNS_MILLIS_KEY = "timeBetweenEvictionRunsMillis";

    Long TIME_BETWEEN_EVICTION_RUNS_MILLIS_DEFAULT = 1L;

    /**
     * The constant URL_BLOCK_WHEN_EXHAUSTED_KEY.
     */
    String URL_BLOCK_WHEN_EXHAUSTED_KEY = "blockWhenExhausted";

    Boolean BLOCK_WHEN_EXHAUSTED_DEFAULT = true;

    /**
     * The constant URL_TIME_OUT_KEY.
     */
    String URL_TIME_OUT_KEY = "timeOut";

    Integer TIME_OUT_DEFAULT = 10000;

    String MODE_DEFAULT = "default";

    String MODE_CLUSTER = "cluster";

    String MODE_SENTINEL = "sentinel";
}
