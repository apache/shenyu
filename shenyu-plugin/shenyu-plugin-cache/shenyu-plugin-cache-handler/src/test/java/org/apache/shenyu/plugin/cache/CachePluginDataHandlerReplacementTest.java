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

package org.apache.shenyu.plugin.cache;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.cache.handler.CachePluginDataHandler;
import org.apache.shenyu.plugin.cache.utils.CacheUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Test cases for the cache replacement of {@link CachePluginDataHandler}.
 *
 * <p>They deliberately do not use the embedded redis of {@code CachePluginDataHandlerTest}: the memory
 * cache is enough to observe the order in which the previous cache is closed and the new one is
 * installed.
 */
public class CachePluginDataHandlerReplacementTest {

    @Test
    public void handlerPluginInstallsTheNewCacheBeforeClosingThePreviousOne() {
        // stand in for the cache that a configuration change replaces
        ICache previousCache = Mockito.mock(ICache.class);
        AtomicReference<ICache> cacheWhileClosing = new AtomicReference<>();
        Mockito.doAnswer(invocation -> {
            cacheWhileClosing.set(CacheUtils.getCache());
            return null;
        }).when(previousCache).close();
        Singleton.INST.single(ICache.class, previousCache);

        final PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        // a config that differs from whatever another test left in the singleton
        pluginData.setConfig("{\"cacheType\":\"memory\",\"probe\":\"" + UUID.randomUUID() + "\"}");

        new CachePluginDataHandler().handlerPlugin(pluginData);

        Mockito.verify(previousCache).close();
        // a cache must never be handed out once its client has been released
        Assertions.assertNotNull(cacheWhileClosing.get());
        Assertions.assertNotSame(previousCache, cacheWhileClosing.get());
    }
}
