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

package org.apache.shenyu.plugin.sign.cache;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.dto.AppAuthData;

import java.util.concurrent.ConcurrentMap;

/**
 * The type Base data cache.
 */
public final class SignAuthDataCache {
    
    private static final SignAuthDataCache INSTANCE = new SignAuthDataCache();
    
    /**
     * appKey -> AppAuthData.
     */
    private static final ConcurrentMap<String, AppAuthData> AUTH_MAP = Maps.newConcurrentMap();
    
    private SignAuthDataCache() {
    }
    
    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static SignAuthDataCache getInstance() {
        return INSTANCE;
    }
    
    /**
     * Cache auth data.
     *
     * @param data the data
     */
    public void cacheAuthData(final AppAuthData data) {
        AUTH_MAP.put(data.getAppKey(), data);
    }
    
    /**
     * Remove auth data.
     *
     * @param data the data
     */
    public void removeAuthData(final AppAuthData data) {
        AUTH_MAP.remove(data.getAppKey());
    }
    
    /**
     * Obtain auth data app auth data.
     *
     * @param appKey the app key
     * @return the app auth data
     */
    public AppAuthData obtainAuthData(final String appKey) {
        return AUTH_MAP.get(appKey);
    }
}
