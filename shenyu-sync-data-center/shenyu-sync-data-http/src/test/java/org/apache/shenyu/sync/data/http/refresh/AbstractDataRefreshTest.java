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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link AbstractDataRefresh}.
 */
public final class AbstractDataRefreshTest {

    private static final ConfigGroupEnum GROUP = ConfigGroupEnum.META_DATA;

    @BeforeEach
    public void clearGroupCache() {
        AbstractDataRefresh.GROUP_CACHE.remove(GROUP);
    }

    @AfterEach
    public void tearDown() {
        AbstractDataRefresh.GROUP_CACHE.remove(GROUP);
    }

    @Test
    public void refreshShouldReturnFalseWhenConvertReturnsNull() {
        StubDataRefresh dataRefresh = new StubDataRefresh();
        dataRefresh.skipConvert = true;

        assertFalse(dataRefresh.refresh(new JsonObject()));
        assertFalse(dataRefresh.refreshed);
    }

    @Test
    public void refreshShouldUpdateWhenGroupCacheIsEmpty() {
        StubDataRefresh dataRefresh = new StubDataRefresh();
        ConfigData<PluginData> config = config("md5-new", 100L);
        dataRefresh.parsed = config;

        assertTrue(dataRefresh.refresh(new JsonObject()));
        assertTrue(dataRefresh.refreshed);
        assertSame(config, AbstractDataRefresh.GROUP_CACHE.get(GROUP));
    }

    @Test
    public void refreshShouldIgnoreSameMd5EvenWithNewerModifyTime() {
        StubDataRefresh dataRefresh = new StubDataRefresh();
        ConfigData<PluginData> original = config("md5-same", 100L);
        dataRefresh.parsed = original;
        assertTrue(dataRefresh.refresh(new JsonObject()));

        dataRefresh.refreshed = false;
        dataRefresh.parsed = config("md5-same", 200L);
        assertFalse(dataRefresh.refresh(new JsonObject()));
        assertFalse(dataRefresh.refreshed);
        assertSame(original, AbstractDataRefresh.GROUP_CACHE.get(GROUP));
    }

    @Test
    public void refreshShouldIgnoreNewerMd5WhenModifyTimeIsNotNewer() {
        StubDataRefresh dataRefresh = new StubDataRefresh();
        ConfigData<PluginData> original = config("md5-old", 200L);
        dataRefresh.parsed = original;
        assertTrue(dataRefresh.refresh(new JsonObject()));

        dataRefresh.refreshed = false;
        dataRefresh.parsed = config("md5-new", 100L);
        assertFalse(dataRefresh.refresh(new JsonObject()));
        assertFalse(dataRefresh.refreshed);
        assertSame(original, AbstractDataRefresh.GROUP_CACHE.get(GROUP));
    }

    @Test
    public void refreshShouldUpdateWhenMd5AndModifyTimeAreNewer() {
        StubDataRefresh dataRefresh = new StubDataRefresh();
        ConfigData<PluginData> original = config("md5-old", 100L);
        dataRefresh.parsed = original;
        assertTrue(dataRefresh.refresh(new JsonObject()));

        dataRefresh.refreshed = false;
        ConfigData<PluginData> latest = config("md5-new", 200L);
        dataRefresh.parsed = latest;
        assertTrue(dataRefresh.refresh(new JsonObject()));
        assertTrue(dataRefresh.refreshed);
        assertSame(latest, AbstractDataRefresh.GROUP_CACHE.get(GROUP));
    }

    private ConfigData<PluginData> config(final String md5, final long lastModifyTime) {
        return new ConfigData<>(md5, lastModifyTime, Collections.<PluginData>emptyList());
    }

    private static final class StubDataRefresh extends AbstractDataRefresh<PluginData> {

        private boolean skipConvert;

        private boolean refreshed;

        private ConfigData<PluginData> parsed;

        @Override
        protected JsonObject convert(final JsonObject data) {
            return skipConvert ? null : data;
        }

        @Override
        protected ConfigData<PluginData> fromJson(final JsonObject data) {
            return parsed;
        }

        @Override
        protected void refresh(final List<PluginData> data) {
            refreshed = true;
        }

        @Override
        protected boolean updateCacheIfNeed(final ConfigData<PluginData> result) {
            return updateCacheIfNeed(result, GROUP);
        }

        @Override
        public ConfigData<?> cacheConfigData() {
            return GROUP_CACHE.get(GROUP);
        }
    }
}
