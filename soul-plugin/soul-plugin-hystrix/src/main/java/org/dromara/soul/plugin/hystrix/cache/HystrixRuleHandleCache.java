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

package org.dromara.soul.plugin.hystrix.cache;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.dto.convert.HystrixHandle;
import org.dromara.soul.plugin.base.cache.BaseHandleCache;

/**
 * The rule handle cache.
 */
@SuppressWarnings("all")
@Slf4j
public final class HystrixRuleHandleCache extends BaseHandleCache<String, HystrixHandle> {

    private HystrixRuleHandleCache() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static HystrixRuleHandleCache getInstance() {
        return HystrixRuleHandleCacheInstance.INSTANCE;
    }

    /**
     * The type rule handle cache instance.
     */
    static class HystrixRuleHandleCacheInstance {
        /**
         * The Instance.
         */
        static final HystrixRuleHandleCache INSTANCE = new HystrixRuleHandleCache();
    }
}
