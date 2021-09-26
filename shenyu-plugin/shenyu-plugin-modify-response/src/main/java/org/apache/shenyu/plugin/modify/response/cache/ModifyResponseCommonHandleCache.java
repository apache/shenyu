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

package org.apache.shenyu.plugin.modify.response.cache;

import org.apache.shenyu.common.dto.convert.rule.impl.ModifyResponseRuleHandle;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;

/**
 * The rule handle cache.
 */
@SuppressWarnings("all")
public final class ModifyResponseCommonHandleCache extends CommonHandleCache<String, ModifyResponseRuleHandle> {

    private ModifyResponseCommonHandleCache() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static ModifyResponseCommonHandleCache getInstance() {
        return ModifyResponseRuleHandleCacheInstance.INSTANCE;
    }

    /**
     * The type rule handle cache instance.
     */
    static class ModifyResponseRuleHandleCacheInstance {
        /**
         * The Instance.
         */
        static final ModifyResponseCommonHandleCache INSTANCE = new ModifyResponseCommonHandleCache();
    }
}
