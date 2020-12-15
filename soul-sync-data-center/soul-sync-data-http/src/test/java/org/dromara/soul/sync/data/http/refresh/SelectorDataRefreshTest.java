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

package org.dromara.soul.sync.data.http.refresh;

import com.google.gson.JsonObject;
import org.dromara.soul.common.dto.ConfigData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Test cases for {@link SelectorDataRefresh}.
 *
 * @author davidliu
 */
public class SelectorDataRefreshTest {
    
    private final SelectorDataRefresh mockSelectorDataRefresh = new SelectorDataRefresh(new PluginDataSubscriber() {
        @Override
        public void onSubscribe(final PluginData pluginData) {
        
        }
    });
    
    
    /**
     * test case for {@link SelectorDataRefresh#convert(JsonObject)}.
     */
    @Test
    public void testConvert() {
        JsonObject jsonObject = new JsonObject();
        JsonObject expectJsonObject = new JsonObject();
        jsonObject.add(ConfigGroupEnum.SELECTOR.name(), expectJsonObject);
        Assert.assertEquals(expectJsonObject, mockSelectorDataRefresh.convert(jsonObject));
    }
    
    /**
     * test case for {@link SelectorDataRefresh#fromJson(JsonObject)}.
     */
    @Test
    public void testFromJson() {
        ConfigData<SelectorData> selectorDataConfigData = new ConfigData<>();
        SelectorData selectorData = new SelectorData();
        selectorDataConfigData.setData(Collections.singletonList(selectorData));
        JsonObject jsonObject = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(selectorDataConfigData), JsonObject.class);
        Assert.assertEquals(selectorDataConfigData, mockSelectorDataRefresh.fromJson(jsonObject));
    }
    
    /**
     * This case coverages the following method:
     * {@link SelectorDataRefresh#cacheConfigData()}
     * {@link SelectorDataRefresh#updateCacheIfNeed(ConfigData)}.
     * For {@link SelectorDataRefresh} inherits from {@link AbstractDataRefresh}, the {@link AbstractDataRefresh#GROUP_CACHE} was initialized when the class of
     * {@link AbstractDataRefresh} load, in two different test methods in this class, the the {@link AbstractDataRefresh#GROUP_CACHE} class only load once, so
     * the method which manipulate the {@link AbstractDataRefresh#GROUP_CACHE} invocation has aftereffects to the other methods.
     */
    @Test
    public void testUpdateCacheIfNeed() {
        final SelectorDataRefresh selectorDataRefresh = mockSelectorDataRefresh;
        // update cache, then assert equals
        ConfigData<SelectorData> expect = new ConfigData<>();
        expect.setLastModifyTime(System.currentTimeMillis());
        selectorDataRefresh.updateCacheIfNeed(expect);
        Assert.assertEquals(expect, selectorDataRefresh.cacheConfigData());
    }
    
    /**
     * This case is only for {@link SelectorDataRefresh} code coverage.
     */
    @Test
    public void testRefreshCoverage() {
        final SelectorDataRefresh selectorDataRefresh = mockSelectorDataRefresh;
        SelectorData selectorData = new SelectorData();
        List<SelectorData> selectorDataList = new ArrayList<>();
        selectorDataRefresh.refresh(selectorDataList);
        selectorDataList.add(selectorData);
        selectorDataRefresh.refresh(selectorDataList);
        
    }
}
