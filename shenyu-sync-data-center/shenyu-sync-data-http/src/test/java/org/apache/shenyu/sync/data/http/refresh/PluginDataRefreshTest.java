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
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class PluginDataRefreshTest {

    private final PluginDataRefresh mockPluginDataRefresh = new PluginDataRefresh(new PluginDataSubscriber() {
        @Override
        public void onSubscribe(final PluginData pluginData) {

        }
    });

    @Test
    public void testConvert() {
        JsonObject jsonObject = new JsonObject();
        JsonObject expectJsonObject = new JsonObject();
        jsonObject.add(ConfigGroupEnum.PLUGIN.name(), expectJsonObject);
        assertThat(mockPluginDataRefresh.convert(jsonObject), is(expectJsonObject));
    }

    @Test
    public void testFromJson() {
        ConfigData<PluginData> pluginDataConfigData = new ConfigData<>();
        PluginData pluginData = new PluginData();
        pluginDataConfigData.setData(Collections.singletonList(pluginData));
        JsonObject jsonObject = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(pluginDataConfigData), JsonObject.class);
        assertThat(mockPluginDataRefresh.fromJson(jsonObject), is(pluginDataConfigData));
    }

    @Test
    public void testUpdateCacheIfNeed() {
        final PluginDataRefresh pluginDataRefresh = mockPluginDataRefresh;
        // update cache, then assert equals
        ConfigData<PluginData> expect = new ConfigData<>();
        expect.setLastModifyTime(System.currentTimeMillis());
        pluginDataRefresh.updateCacheIfNeed(expect);
        assertThat(pluginDataRefresh.cacheConfigData(), is(expect));
    }

    @Test
    public void testRefreshCoverage() {
        final PluginDataRefresh pluginDataRefresh = mockPluginDataRefresh;
        PluginData selectorData = new PluginData();
        List<PluginData> selectorDataList = new ArrayList<>();
        pluginDataRefresh.refresh(selectorDataList);
        selectorDataList.add(selectorData);
        pluginDataRefresh.refresh(selectorDataList);
    }
}
