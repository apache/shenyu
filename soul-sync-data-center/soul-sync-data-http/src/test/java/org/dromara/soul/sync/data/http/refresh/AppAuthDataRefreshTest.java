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
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.ConfigData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Test cases for {@link AppAuthDataRefresh}.
 *
 * @author davidliu
 */
public class AppAuthDataRefreshTest {
    
    private final AppAuthDataRefresh mockAppAuthDataRefresh = this.buildMockAppAuthDataRefresh();
    
    /**
     * test case for {@link AppAuthDataRefresh#convert(JsonObject)}.
     */
    @Test
    public void testConvert() {
        JsonObject jsonObject = new JsonObject();
        JsonObject expectJsonObject = new JsonObject();
        jsonObject.add(ConfigGroupEnum.APP_AUTH.name(), expectJsonObject);
        Assert.assertEquals(expectJsonObject, mockAppAuthDataRefresh.convert(jsonObject));
    }
    
    /**
     * test case for {@link AppAuthDataRefresh#fromJson(JsonObject)}.
     */
    @Test
    public void testFromJson() {
        ConfigData<AppAuthData> appAuthDataConfigData = new ConfigData<>();
        AppAuthData appAuthData = new AppAuthData();
        appAuthDataConfigData.setData(Collections.singletonList(appAuthData));
        JsonObject jsonObject = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(appAuthDataConfigData), JsonObject.class);
        Assert.assertEquals(appAuthDataConfigData, mockAppAuthDataRefresh.fromJson(jsonObject));
    }
    
    /**
     * This case coverages the following method:
     * {@link AppAuthDataRefresh#cacheConfigData()}
     * {@link AppAuthDataRefresh#updateCacheIfNeed(ConfigData)}.
     * For {@link SelectorDataRefresh} inherits from {@link AbstractDataRefresh}, the {@link AbstractDataRefresh#GROUP_CACHE} was initialized when the class of
     * {@link AbstractDataRefresh} load, in two different test methods in this class, the the {@link AbstractDataRefresh#GROUP_CACHE} class only load once, so
     * the method which manipulate the {@link AbstractDataRefresh#GROUP_CACHE} invocation has aftereffects to the other methods.
     */
    @Test
    public void testUpdateCacheIfNeed() {
        final AppAuthDataRefresh appAuthDataRefresh = mockAppAuthDataRefresh;
        // first, expect getting null from cache
        Assert.assertNull(appAuthDataRefresh.cacheConfigData());
        // update cache, then assert equals
        ConfigData<AppAuthData> expect = new ConfigData<>();
        Assert.assertTrue(appAuthDataRefresh.updateCacheIfNeed(expect));
        Assert.assertEquals(expect, appAuthDataRefresh.cacheConfigData());
    }
    
    /**
     * This case is only for {@link AppAuthDataRefresh} code coverage.
     */
    @Test
    public void testRefreshCoverage() {
        final AppAuthDataRefresh appAuthDataRefresh = mockAppAuthDataRefresh;
        AppAuthData appAuthData = new AppAuthData();
        List<AppAuthData> appAuthDataList = new ArrayList<>();
        appAuthDataRefresh.refresh(appAuthDataList);
        appAuthDataList.add(appAuthData);
        appAuthDataRefresh.refresh(appAuthDataList);
        
    }
    
    private AppAuthDataRefresh buildMockAppAuthDataRefresh() {
        List<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        authDataSubscribers.add(new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
            
            }
            
            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
            
            }
        });
        return new AppAuthDataRefresh(authDataSubscribers);
    }
    
}
