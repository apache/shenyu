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

package org.apache.shenyu.plugin.base.cache;

import java.lang.reflect.Field;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * BaseHandleCache test.
 */
public final class CommonHandleCacheTest {

    private static final String MOCK_KEY = "mockKey";

    private static final String MOCK_VALUE = "mockValue";

    private CommonHandleCache<String, String> commonHandleCache;

    private ConcurrentHashMap<String, String> cachedMockMap;

    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException {
        this.commonHandleCache = new EmptyMockCommonHandleCache<>();
        this.cachedMockMap = new ConcurrentHashMap<>();

        Field field = this.commonHandleCache.getClass().getSuperclass().getDeclaredField("cached");
        field.setAccessible(true);
        field.set(commonHandleCache, cachedMockMap);
    }

    @Test
    public void obtainHandle() {
        assert null == commonHandleCache.obtainHandle(MOCK_KEY);
        cachedMockMap.put(MOCK_KEY, MOCK_VALUE);
        assert MOCK_VALUE.equals(commonHandleCache.obtainHandle(MOCK_KEY));
    }

    @Test
    public void cachedHandle() {
        commonHandleCache.cachedHandle(null, MOCK_VALUE);
        assert cachedMockMap.size() == 0;
        commonHandleCache.cachedHandle(MOCK_KEY, MOCK_VALUE);
        assert MOCK_VALUE.equals(commonHandleCache.obtainHandle(MOCK_KEY));
    }

    @Test
    public void removeHandle() {
        commonHandleCache.cachedHandle(MOCK_KEY, MOCK_VALUE);
        assert cachedMockMap.size() == 1;
        commonHandleCache.removeHandle(MOCK_KEY);
        assert cachedMockMap.size() == 0;
    }
}
