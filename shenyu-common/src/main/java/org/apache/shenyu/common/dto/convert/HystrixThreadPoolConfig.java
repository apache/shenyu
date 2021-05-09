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

package org.apache.shenyu.common.dto.convert;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.apache.shenyu.common.constant.Constants;

/**
 * hystrix thread pool config.
 */
@Getter
@Setter
@EqualsAndHashCode
public class HystrixThreadPoolConfig {
    private int coreSize = Constants.HYSTRIX_THREAD_POOL_CORE_SIZE;

    private int maximumSize = Constants.HYSTRIX_THREAD_POOL_MAX_SIZE;

    private int keepAliveTimeMinutes = Constants.HYSTRIX_THREAD_KEEP_ALIVE_TIME_MINUTE;

    private int maxQueueSize = Constants.HYSTRIX_THREAD_POOL_QUEUE_SIZE;
}
