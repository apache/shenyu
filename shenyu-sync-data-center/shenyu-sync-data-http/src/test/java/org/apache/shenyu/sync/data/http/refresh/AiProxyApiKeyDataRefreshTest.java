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
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * Test cases for {@link AiProxyApiKeyDataRefresh}.
 */
public final class AiProxyApiKeyDataRefreshTest {

    private final AiProxyApiKeyDataSubscriber subscriber = mock(AiProxyApiKeyDataSubscriber.class);

    private final AiProxyApiKeyDataRefresh dataRefresh =
            new AiProxyApiKeyDataRefresh(Collections.singletonList(subscriber));

    @BeforeEach
    public void clearGroupCache() {
        AbstractDataRefresh.GROUP_CACHE.remove(ConfigGroupEnum.AI_PROXY_API_KEY);
    }

    @AfterEach
    public void tearDown() {
        AbstractDataRefresh.GROUP_CACHE.remove(ConfigGroupEnum.AI_PROXY_API_KEY);
    }

    @Test
    public void convertShouldReturnGroupJson() {
        JsonObject data = new JsonObject();
        JsonObject groupJson = new JsonObject();
        data.add(ConfigGroupEnum.AI_PROXY_API_KEY.name(), groupJson);

        assertEquals(groupJson, dataRefresh.convert(data));
        assertNull(dataRefresh.convert(new JsonObject()));
    }

    @Test
    public void fromJsonShouldParseConfigData() {
        ProxyApiKeyData apiKeyData = ProxyApiKeyData.builder()
                .realApiKey("real-key")
                .proxyApiKey("proxy-key")
                .enabled(true)
                .build();
        ConfigData<ProxyApiKeyData> config =
                new ConfigData<>("md5", 100L, Collections.singletonList(apiKeyData));
        JsonObject json = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(config), JsonObject.class);

        assertEquals(config, dataRefresh.fromJson(json));
    }

    @Test
    public void refreshShouldClearThenSubscribeAllItems() {
        ProxyApiKeyData first = ProxyApiKeyData.builder().proxyApiKey("first").build();
        ProxyApiKeyData second = ProxyApiKeyData.builder().proxyApiKey("second").build();

        dataRefresh.refresh(List.of(first, second));

        verify(subscriber).refresh();
        verify(subscriber).onSubscribe(first);
        verify(subscriber).onSubscribe(second);
    }

    @Test
    public void refreshWithEmptyListShouldOnlyClear() {
        dataRefresh.refresh(Collections.emptyList());

        verify(subscriber).refresh();
        verify(subscriber, never()).onSubscribe(any());
    }

    @Test
    public void refreshShouldTolerateNoSubscribers() {
        AiProxyApiKeyDataRefresh emptyRefresh =
                new AiProxyApiKeyDataRefresh(Collections.emptyList());

        assertDoesNotThrow(() -> emptyRefresh.refresh(Collections.singletonList(new ProxyApiKeyData())));
    }

    @Test
    public void refreshJsonShouldUpdateCacheAndNotifySubscribers() {
        ProxyApiKeyData apiKeyData = ProxyApiKeyData.builder().proxyApiKey("proxy-key").build();
        ConfigData<ProxyApiKeyData> config = new ConfigData<>("md5-new", System.currentTimeMillis(),
                Collections.singletonList(apiKeyData));
        JsonObject groupJson = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(config), JsonObject.class);
        JsonObject data = new JsonObject();
        data.add(ConfigGroupEnum.AI_PROXY_API_KEY.name(), groupJson);

        assertTrue(dataRefresh.refresh(data));
        verify(subscriber).refresh();
        verify(subscriber).onSubscribe(apiKeyData);
        assertEquals(config, dataRefresh.cacheConfigData());
    }

    @Test
    public void cacheConfigDataShouldBeEmptyBeforeFirstRefresh() {
        assertFalse(dataRefresh.refresh(new JsonObject()));
        assertNull(dataRefresh.cacheConfigData());
    }
}
