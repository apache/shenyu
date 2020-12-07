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

package org.dromara.soul.sync.data.nacos.handler;

import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.JSON;
import com.alibaba.nacos.api.config.ConfigFactory;
import com.alibaba.nacos.api.config.ConfigService;
import com.google.common.collect.Lists;
import lombok.SneakyThrows;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.junit.Test;
import org.junit.Before;
import org.junit.Assert;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * Test cases for {@link NacosCacheHandler}.
 *
 * @author kminjava
 */
@SuppressWarnings("all")
public class NacosCacheHandlerTest {

    private NacosCacheHandler nacosCacheHandlerService;

    private ConfigService configService;

    @Before
    public void setUp() throws Exception {
        configService = ConfigFactory.createConfigService("127.0.0.1:8848");
    }

    /**
     * test case for {@link NacosCacheHandler#updatePluginMap(String)}.
     */
    @SneakyThrows
    @Test
    public void testUpdatePluginMap() {
        //init nacos plugin data
        final String configInfo = mockConfigsFetchResponseJson();
        JSONObject object = JSON.parseObject(configInfo);
        final String pulginData = object.getString("pluginData");
        final CountDownLatch latch = new CountDownLatch(2);
        final List<PluginData> onSubscribeList = new ArrayList<>();
        final List<PluginData> unsubscribeList = new ArrayList<>();
        nacosCacheHandlerService = new NacosCacheHandler(configService, new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
                onSubscribeList.add(pluginData);
                latch.countDown();
            }

            @Override
            public void unSubscribe(final PluginData pluginData) {
                unsubscribeList.add(pluginData);
                latch.countDown();
            }
        }, Collections.emptyList(), Collections.emptyList());
        nacosCacheHandlerService.updatePluginMap(pulginData);
        latch.await(500, TimeUnit.MILLISECONDS);
        Assert.assertEquals(9, onSubscribeList.size());
        Assert.assertEquals(9, unsubscribeList.size());

    }

    /**
     * test case for {@link NacosCacheHandler#updateSelectorMap(String)}.
     */
    @SneakyThrows
    @Test
    public void testUpdateSelectorMap() {
        final String configInfo = mockConfigsFetchResponseJson();
        JSONObject object = JSON.parseObject(configInfo);
        final String selectorDataParam = object.getString("selectorData");

        final CountDownLatch latch = new CountDownLatch(2);
        final List<SelectorData> subscribeList = new ArrayList<>();
        final List<SelectorData> unsubscribeList = new ArrayList<>();
        nacosCacheHandlerService = new NacosCacheHandler(configService, new PluginDataSubscriber() {
            @Override
            public void onSelectorSubscribe(final SelectorData selectorData) {
                subscribeList.add(selectorData);
                latch.countDown();
            }

            @Override
            public void unSelectorSubscribe(final SelectorData selectorData) {
                unsubscribeList.add(selectorData);
                latch.countDown();
            }
        }, Collections.emptyList(), Collections.emptyList());
        nacosCacheHandlerService.updateSelectorMap(selectorDataParam);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
    }

    /**
     * test case for {@link NacosCacheHandler#updateRuleMap(String)}.
     */
    @SneakyThrows
    @Test
    public void testUpdateRuleMap() {
        final CountDownLatch latch = new CountDownLatch(2);
        final List<RuleData> subscribeList = new ArrayList<>();
        final List<RuleData> unsubscribeList = new ArrayList<>();
        // init ruleData
        final String configInfo = mockConfigsFetchResponseJson();
        JSONObject object = JSON.parseObject(configInfo);
        final String ruleDataParam = object.getString("ruleData");
        nacosCacheHandlerService = new NacosCacheHandler(configService, new PluginDataSubscriber() {
            @Override
            public void onRuleSubscribe(final RuleData ruleData) {
                subscribeList.add(ruleData);
                latch.countDown();
            }

            @Override
            public void unRuleSubscribe(final RuleData ruleData) {
                unsubscribeList.add(ruleData);
                latch.countDown();
            }
        }, Collections.emptyList(), Collections.emptyList());
        nacosCacheHandlerService.updateRuleMap(ruleDataParam);
        latch.await(10, TimeUnit.SECONDS);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
    }

    /**
     * test case for {@link NacosCacheHandler#updateMetaDataMap(String)}.
     */
    @SneakyThrows
    @Test
    public void testUpdateMetaDataMap() {
        final String configInfo = mockConfigsFetchResponseJson();
        JSONObject object = JSON.parseObject(configInfo);
        //init metaData
        final String metaDataParam = object.getString("metaData");
        final CountDownLatch latch = new CountDownLatch(2);
        final List<MetaData> subscribeList = new ArrayList<>();
        final List<MetaData> unsubscribeList = new ArrayList<>();
        MetaDataSubscriber metaDataSubscriber = new MetaDataSubscriber() {
            @Override
            public void onSubscribe(final MetaData metaData) {
                subscribeList.add(metaData);
                latch.countDown();
            }

            @Override
            public void unSubscribe(final MetaData metaData) {
                unsubscribeList.add(metaData);
                latch.countDown();
            }
        };
        nacosCacheHandlerService = new NacosCacheHandler(configService, null, Lists.newArrayList(metaDataSubscriber),
                Collections.emptyList());
        nacosCacheHandlerService.updateMetaDataMap(metaDataParam);
        latch.await(500, TimeUnit.MILLISECONDS);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
    }

    /**
     * test case for {@link NacosCacheHandler#updateAuthMap(String)}.
     */
    @SneakyThrows
    @Test
    public void testUpdateAuthMap() {
        final CountDownLatch latch = new CountDownLatch(2);
        final List<AppAuthData> subscribeList = new ArrayList<>();
        final List<AppAuthData> unsubscribeList = new ArrayList<>();
        //init appAuthdData
        final String configInfo = mockConfigsFetchResponseJson();
        JSONObject object = JSON.parseObject(configInfo);
        final String appAuthDataParam = object.getString("appAuthData");

        AuthDataSubscriber authDataSubscriber = new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
                subscribeList.add(appAuthData);
                latch.countDown();
            }

            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
                unsubscribeList.add(appAuthData);
                latch.countDown();
            }
        };
        nacosCacheHandlerService = new NacosCacheHandler(configService, null,
                Collections.emptyList(), Lists.newArrayList(authDataSubscriber));

        nacosCacheHandlerService.updateAuthMap(appAuthDataParam);
        latch.await(500, TimeUnit.MILLISECONDS);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
    }

    // mock configs fetch api response
    private String mockConfigsFetchResponseJson() {
        try (FileInputStream fis = new FileInputStream(this.getClass().getClassLoader().getResource("plugin.json").getPath());
             InputStreamReader reader = new InputStreamReader(fis);
             BufferedReader bufferedReader = new BufferedReader(reader);
        ) {
            StringBuilder builder = new StringBuilder();
            bufferedReader.lines().forEach(builder::append);
            return builder.toString();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

}
