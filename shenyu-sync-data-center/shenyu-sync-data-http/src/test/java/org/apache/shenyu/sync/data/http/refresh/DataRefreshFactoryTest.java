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

package org.apache.shenyu.sync.data.http.refresh;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * Test cases for {@link DataRefreshFactory}.
 */
public final class DataRefreshFactoryTest {

    private final PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);

    private final DataRefreshFactory dataRefreshFactory;

    public DataRefreshFactoryTest() {
        dataRefreshFactory = new DataRefreshFactory(pluginDataSubscriber,
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList());
    }

    @BeforeEach
    public void clearPluginCache() {
        AbstractDataRefresh.GROUP_CACHE.remove(ConfigGroupEnum.PLUGIN);
    }

    @AfterEach
    public void tearDown() {
        AbstractDataRefresh.GROUP_CACHE.remove(ConfigGroupEnum.PLUGIN);
    }

    @Test
    public void executorShouldReturnFalseWhenNoGroupDataIsPresent() {
        assertFalse(dataRefreshFactory.executor(new JsonObject()));
        assertNull(dataRefreshFactory.cacheConfigData(ConfigGroupEnum.PLUGIN));
    }

    @Test
    public void executorShouldRefreshRegisteredGroup() {
        PluginData pluginData = PluginData.builder().name("sign-plugin").enabled(true).build();
        ConfigData<PluginData> config = new ConfigData<>("md5-new", System.currentTimeMillis(),
                Collections.singletonList(pluginData));
        JsonObject groupJson = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(config), JsonObject.class);
        JsonObject data = new JsonObject();
        data.add(ConfigGroupEnum.PLUGIN.name(), groupJson);

        assertTrue(dataRefreshFactory.executor(data));

        verify(pluginDataSubscriber).refreshPluginDataAll();
        verify(pluginDataSubscriber).onSubscribe(pluginData);
        assertEquals("md5-new", dataRefreshFactory.cacheConfigData(ConfigGroupEnum.PLUGIN).getMd5());
    }

    @Test
    public void executorShouldReturnFalseWhenTheSameConfigIsRepeated() {
        PluginData pluginData = PluginData.builder().name("sign-plugin").build();
        long lastModifyTime = System.currentTimeMillis();
        ConfigData<PluginData> config = new ConfigData<>("md5-same", lastModifyTime,
                Collections.singletonList(pluginData));
        JsonObject groupJson = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(config), JsonObject.class);
        JsonObject data = new JsonObject();
        data.add(ConfigGroupEnum.PLUGIN.name(), groupJson);

        assertTrue(dataRefreshFactory.executor(data));
        assertFalse(dataRefreshFactory.executor(data));
        verify(pluginDataSubscriber).refreshPluginDataAll();
        verify(pluginDataSubscriber).onSubscribe(pluginData);
        verify(pluginDataSubscriber, never()).unSubscribe(any());
    }
}
