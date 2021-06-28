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

package org.apache.shenyu.plugin.sync.data.websocket.handler;

import com.google.gson.Gson;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.Test;

import java.util.LinkedList;
import java.util.List;

import static org.junit.Assert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public final class PluginDataHandlerTest {

    private final PluginDataSubscriber subscriber;

    private final PluginDataHandler pluginDataHandler;

    public PluginDataHandlerTest() {
        subscriber = mock(PluginDataSubscriber.class);
        pluginDataHandler = new PluginDataHandler(subscriber);
    }

    @Test
    public void testConvert() {
        List<PluginData> pluginDataList = new LinkedList<>();
        pluginDataList.add(PluginData.builder().name("name1").enabled(true).build());
        pluginDataList.add(PluginData.builder().name("name2").role("0").build());
        Gson gson = new Gson();
        String json = gson.toJson(pluginDataList);
        List<PluginData> convertedList = pluginDataHandler.convert(json);
        assertThat(convertedList, is(pluginDataList));
    }

    @Test
    public void testDoRefresh() {
        List<PluginData> pluginDataList = createFakePluginDataObjects(3);
        pluginDataHandler.doRefresh(pluginDataList);
        verify(subscriber).refreshPluginDataSelf(pluginDataList);
        pluginDataList.forEach(verify(subscriber)::onSubscribe);
    }

    @Test
    public void testDoUpdate() {
        List<PluginData> pluginDataList = createFakePluginDataObjects(4);
        pluginDataHandler.doUpdate(pluginDataList);
        pluginDataList.forEach(verify(subscriber)::onSubscribe);
    }

    @Test
    public void testDoDelete() {
        List<PluginData> pluginDataList = createFakePluginDataObjects(3);
        pluginDataHandler.doDelete(pluginDataList);
        pluginDataList.forEach(verify(subscriber)::unSubscribe);
    }

    private List<PluginData> createFakePluginDataObjects(final int count) {
        List<PluginData> result = new LinkedList<>();
        for (int i = 1; i <= count; i++) {
            result.add(PluginData.builder().name("name-" + i).role("").build());
        }
        return result;
    }
}
