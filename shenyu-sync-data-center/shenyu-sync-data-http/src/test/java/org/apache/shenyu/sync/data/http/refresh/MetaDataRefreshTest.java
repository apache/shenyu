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
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public final class MetaDataRefreshTest {

    private final MetaDataRefresh mockMetaDataRefresh = this.buildMockMetaDataRefresh();

    @Test
    public void testConvert() {
        JsonObject jsonObject = new JsonObject();
        JsonObject expectJsonObject = new JsonObject();
        jsonObject.add(ConfigGroupEnum.META_DATA.name(), expectJsonObject);
        assertThat(mockMetaDataRefresh.convert(jsonObject), is(expectJsonObject));
    }

    @Test
    public void testFromJson() {
        ConfigData<MetaData> metaDataConfigData = new ConfigData<>();
        MetaData metaData = new MetaData();
        metaDataConfigData.setData(Collections.singletonList(metaData));
        JsonObject jsonObject = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(metaDataConfigData), JsonObject.class);
        assertThat(mockMetaDataRefresh.fromJson(jsonObject), is(metaDataConfigData));
    }

    @Test
    public void testUpdateCacheIfNeed() {
        final MetaDataRefresh metaDataRefresh = mockMetaDataRefresh;
        // update cache, then assert equals
        ConfigData<MetaData> expect = new ConfigData<>();
        expect.setLastModifyTime(System.currentTimeMillis());
        metaDataRefresh.updateCacheIfNeed(expect);
        assertThat(metaDataRefresh.cacheConfigData(), is(expect));
    }

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
