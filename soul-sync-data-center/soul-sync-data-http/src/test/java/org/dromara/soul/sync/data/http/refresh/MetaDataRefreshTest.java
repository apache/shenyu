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
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Test cases for {@link MetaDataRefresh}.
 *
 * @author davidliu
 */
public class MetaDataRefreshTest {
    
    private final MetaDataRefresh mockMetaDataRefresh = this.buildMockMetaDataRefresh();
    
    /**
     * test case for {@link MetaDataRefresh#convert(JsonObject)}.
     */
    @Test
    public void testConvert() {
        JsonObject jsonObject = new JsonObject();
        JsonObject expectJsonObject = new JsonObject();
        jsonObject.add(ConfigGroupEnum.META_DATA.name(), expectJsonObject);
        Assert.assertEquals(expectJsonObject, mockMetaDataRefresh.convert(jsonObject));
    }
    
    /**
     * test case for {@link MetaDataRefresh#fromJson(JsonObject)}.
     */
    @Test
    public void testFromJson() {
        ConfigData<MetaData> metaDataConfigData = new ConfigData<>();
        MetaData metaData = new MetaData();
        metaDataConfigData.setData(Collections.singletonList(metaData));
        JsonObject jsonObject = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(metaDataConfigData), JsonObject.class);
        Assert.assertEquals(metaDataConfigData, mockMetaDataRefresh.fromJson(jsonObject));
    }
    
    /**
     * This case coverages the following method:
     * {@link MetaDataRefresh#cacheConfigData()}
     * {@link MetaDataRefresh#updateCacheIfNeed(ConfigData)}.
     * For {@link SelectorDataRefresh} inherits from {@link AbstractDataRefresh}, the {@link AbstractDataRefresh#GROUP_CACHE} was initialized when the class of
     * {@link AbstractDataRefresh} load, in two different test methods in this class, the the {@link AbstractDataRefresh#GROUP_CACHE} class only load once, so
     * the method which manipulate the {@link AbstractDataRefresh#GROUP_CACHE} invocation has aftereffects to the other methods.
     */
    @Test
    public void testUpdateCacheIfNeed() {
        final MetaDataRefresh metaDataRefresh = mockMetaDataRefresh;
        // update cache, then assert equals
        ConfigData<MetaData> expect = new ConfigData<>();
        expect.setLastModifyTime(System.currentTimeMillis());
        metaDataRefresh.updateCacheIfNeed(expect);
        Assert.assertEquals(expect, metaDataRefresh.cacheConfigData());
    }
    
    /**
     * This case is only for {@link MetaDataRefresh} code coverage.
     */
    @Test
    public void testRefreshCoverage() {
        final MetaDataRefresh metaDataRefresh = mockMetaDataRefresh;
        MetaData metaData = new MetaData();
        List<MetaData> metaDataList = new ArrayList<>();
        metaDataRefresh.refresh(metaDataList);
        metaDataList.add(metaData);
        metaDataRefresh.refresh(metaDataList);
        
    }
    
    private MetaDataRefresh buildMockMetaDataRefresh() {
        List<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        metaDataSubscribers.add(new MetaDataSubscriber() {
            @Override
            public void onSubscribe(final MetaData metaData) {
            
            }
            
            @Override
            public void unSubscribe(final MetaData metaData) {
            
            }
        });
        return new MetaDataRefresh(metaDataSubscribers);
    }
    
}
