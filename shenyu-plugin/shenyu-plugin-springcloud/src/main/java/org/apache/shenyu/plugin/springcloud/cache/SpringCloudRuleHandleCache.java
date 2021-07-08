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

package org.apache.shenyu.plugin.springcloud.cache;

import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.apache.shenyu.plugin.base.cache.RuleHandleCache;

/**
 * The rule handle cache.
 */
@Slf4j
public final class SpringCloudRuleHandleCache extends RuleHandleCache<String, SpringCloudRuleHandle> {

    private SpringCloudRuleHandleCache() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static SpringCloudRuleHandleCache getInstance() {
        return SpringCloudRuleHandleCacheInstance.INSTANCE;
    }

    /**
     * The type rule handle cache instance.
     */
    static class SpringCloudRuleHandleCacheInstance {
        /**
         * The Instance.
         */
        static final SpringCloudRuleHandleCache INSTANCE = new SpringCloudRuleHandleCache();
    }
}
