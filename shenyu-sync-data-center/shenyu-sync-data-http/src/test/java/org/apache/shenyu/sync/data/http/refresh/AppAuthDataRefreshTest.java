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
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public final class AppAuthDataRefreshTest {

    private final AppAuthDataRefresh mockAppAuthDataRefresh = this.buildMockAppAuthDataRefresh();

    @Test
    public void testConvert() {
        JsonObject jsonObject = new JsonObject();
        JsonObject expectJsonObject = new JsonObject();
        jsonObject.add(ConfigGroupEnum.APP_AUTH.name(), expectJsonObject);
        assertThat(mockAppAuthDataRefresh.convert(jsonObject), is(expectJsonObject));
    }

    @Test
    public void testFromJson() {
        ConfigData<AppAuthData> appAuthDataConfigData = new ConfigData<>();
        AppAuthData appAuthData = new AppAuthData();
        appAuthDataConfigData.setData(Collections.singletonList(appAuthData));
        JsonObject jsonObject = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(appAuthDataConfigData), JsonObject.class);
        assertThat(mockAppAuthDataRefresh.fromJson(jsonObject), is(appAuthDataConfigData));
    }

    @Test
    public void testUpdateCacheIfNeed() {
        final AppAuthDataRefresh appAuthDataRefresh = mockAppAuthDataRefresh;
        // update cache, then assert equals
        ConfigData<AppAuthData> expect = new ConfigData<>();
        expect.setLastModifyTime(System.currentTimeMillis());
        appAuthDataRefresh.updateCacheIfNeed(expect);
        assertThat(appAuthDataRefresh.cacheConfigData(), is(expect));
    }

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
