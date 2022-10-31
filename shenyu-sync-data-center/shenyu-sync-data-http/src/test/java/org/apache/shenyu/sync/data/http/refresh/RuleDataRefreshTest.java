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
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public final class RuleDataRefreshTest {

    private final RuleDataRefresh mockRuleDataRefresh = new RuleDataRefresh(new PluginDataSubscriber() {
        @Override
        public void onSubscribe(final PluginData pluginData) {

        }
    });

    @Test
    public void testConvert() {
        JsonObject jsonObject = new JsonObject();
        JsonObject expectJsonObject = new JsonObject();
        jsonObject.add(ConfigGroupEnum.RULE.name(), expectJsonObject);
        assertThat(mockRuleDataRefresh.convert(jsonObject), is(expectJsonObject));
    }

    @Test
    public void testFromJson() {
        ConfigData<RuleData> ruleDataConfigData = new ConfigData<>();
        RuleData ruleData = new RuleData();
        ruleDataConfigData.setData(Collections.singletonList(ruleData));
        JsonObject jsonObject = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(ruleDataConfigData), JsonObject.class);
        assertThat(mockRuleDataRefresh.fromJson(jsonObject), is(ruleDataConfigData));
    }

    @Test
    public void testUpdateCacheIfNeed() {
        final RuleDataRefresh ruleDataRefresh = mockRuleDataRefresh;
        // update cache, then assert equals
        ConfigData<RuleData> expect = new ConfigData<>();
        expect.setLastModifyTime(System.currentTimeMillis());
        ruleDataRefresh.updateCacheIfNeed(expect);
        assertThat(ruleDataRefresh.cacheConfigData(), is(expect));
    }

    @Test
    public void testRefresh() {
        final RuleDataRefresh ruleDataRefresh = mockRuleDataRefresh;
        RuleData ruleData = new RuleData();
        List<RuleData> ruleDataList = new ArrayList<>();
        ruleDataRefresh.refresh(ruleDataList);
        ruleDataList.add(ruleData);
        ruleDataRefresh.refresh(ruleDataList);
    }
}
