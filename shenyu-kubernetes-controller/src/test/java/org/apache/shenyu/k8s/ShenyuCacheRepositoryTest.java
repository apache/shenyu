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

package org.apache.shenyu.k8s;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.CommonDiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.divide.handler.DivideUpstreamDataHandler;
import org.apache.shenyu.plugin.global.subsciber.MetaDataCacheSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.List;

/**
 * Regression tests for upstream snapshot replacement in ShenyuCacheRepository.
 *
 * <p>UpstreamCacheManager.submit merges by URL (removal only for entries explicitly marked
 * offline), so a full-snapshot save whose upstream set shrank must first drop the cached
 * list, otherwise a scaled-down backend keeps receiving traffic forever.
 */
public final class ShenyuCacheRepositoryTest {

    private static final String SELECTOR_ID = "gwapi-upstream-snapshot-test";

    private final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();

    @AfterEach
    public void tearDown() {
        upstreamCacheManager.removeByKey(SELECTOR_ID);
    }

    private ShenyuCacheRepository repository() {
        CommonDiscoveryUpstreamDataSubscriber discoverySubscriber =
                new CommonDiscoveryUpstreamDataSubscriber(List.of(new DivideUpstreamDataHandler()));
        return new ShenyuCacheRepository(Mockito.mock(CommonPluginDataSubscriber.class), discoverySubscriber,
                Mockito.mock(MetaDataSubscriber.class), Mockito.mock(MetaDataCacheSubscriber.class));
    }

    private Upstream upstream(final String url) {
        return Upstream.builder().url(url).weight(50).warmup(0).protocol("http://")
                .healthCheckEnabled(false).status(true).build();
    }

    private SelectorData selector(final String handle) {
        return SelectorData.builder()
                .id(SELECTOR_ID)
                .name("demo-route")
                .pluginName(PluginEnum.DIVIDE.getName())
                .handle(handle)
                .build();
    }

    private String handleOf(final String... urls) {
        StringBuilder handle = new StringBuilder("[");
        for (int i = 0; i < urls.length; i++) {
            if (i > 0) {
                handle.append(",");
            }
            handle.append("{\"weight\":50,\"warmup\":0,\"protocol\":\"http://\",\"upstreamHost\":\"\",")
                    .append("\"upstreamUrl\":\"").append(urls[i]).append("\",\"status\":true,\"timestamp\":0}");
        }
        return handle.append("]").toString();
    }

    @Test
    public void testScaledDownUpstreamIsRemovedFromCache() {
        upstreamCacheManager.submit(SELECTOR_ID, List.of(upstream("10.0.0.1:8189"), upstream("10.0.0.2:8189")));
        Assertions.assertEquals(2, upstreamCacheManager.findUpstreamListBySelectorId(SELECTOR_ID).size());

        repository().saveOrUpdateSelectorData(selector(handleOf("10.0.0.1:8189")));

        List<Upstream> after = upstreamCacheManager.findUpstreamListBySelectorId(SELECTOR_ID);
        Assertions.assertEquals(1, after.size(), "scaled-down upstream must be evicted from the cache");
        Assertions.assertEquals("10.0.0.1:8189", after.get(0).getUrl());
    }

    @Test
    public void testUnchangedSnapshotKeepsUpstreams() {
        upstreamCacheManager.submit(SELECTOR_ID, List.of(upstream("10.0.0.1:8189")));

        repository().saveOrUpdateSelectorData(selector(handleOf("10.0.0.1:8189")));

        List<Upstream> after = upstreamCacheManager.findUpstreamListBySelectorId(SELECTOR_ID);
        Assertions.assertEquals(1, after.size());
        Assertions.assertEquals("10.0.0.1:8189", after.get(0).getUrl());
    }

    /**
     * deleteSelectorWithRules must remove every rule and then the selector even though
     * findRuleDataList exposes BaseDataCache's mutable internal list that each
     * deleteRuleData call mutates — iterating over a copy is what keeps this from
     * throwing ConcurrentModificationException halfway through the cleanup.
     */
    @Test
    public void testDeleteSelectorWithRulesClearsRulesAndSelector() {
        CommonPluginDataSubscriber pluginSubscriber = new CommonPluginDataSubscriber(
                new ArrayList<>(), Mockito.mock(org.springframework.context.ApplicationEventPublisher.class),
                new ShenyuConfig.SelectorMatchCache(), new ShenyuConfig.RuleMatchCache());
        ShenyuCacheRepository repository = new ShenyuCacheRepository(pluginSubscriber,
                Mockito.mock(CommonDiscoveryUpstreamDataSubscriber.class),
                Mockito.mock(MetaDataSubscriber.class), Mockito.mock(MetaDataCacheSubscriber.class));
        String selectorId = "gwapi-cascade-delete-test";
        BaseDataCache.getInstance().cacheRuleData(RuleData.builder()
                .id("rule-1").selectorId(selectorId).pluginName(PluginEnum.DIVIDE.getName()).sort(1).build());
        BaseDataCache.getInstance().cacheRuleData(RuleData.builder()
                .id("rule-2").selectorId(selectorId).pluginName(PluginEnum.DIVIDE.getName()).sort(2).build());

        repository.deleteSelectorWithRules(PluginEnum.DIVIDE.getName(), selectorId);

        Assertions.assertTrue(BaseDataCache.getInstance().obtainRuleData(selectorId).isEmpty());
    }
}
