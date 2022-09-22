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
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.junit.jupiter.api.Test;

import java.util.LinkedList;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public final class MetaDataHandlerTest {

    private final List<MetaDataSubscriber> subscribers;

    private final MetaDataHandler metaDataHandler;

    public MetaDataHandlerTest() {
        subscribers = new LinkedList<>();
        subscribers.add(mock(MetaDataSubscriber.class));
        subscribers.add(mock(MetaDataSubscriber.class));
        subscribers.add(mock(MetaDataSubscriber.class));
        metaDataHandler = new MetaDataHandler(subscribers);
    }

    @Test
    public void testConvert() {
        List<MetaData> metaDataList = new LinkedList<>();
        metaDataList.add(MetaData.builder().id("1").appName("appName1").enabled(true).build());
        metaDataList.add(MetaData.builder().id("1").appName("appName2").methodName("POST").build());
        Gson gson = new Gson();
        String json = gson.toJson(metaDataList);
        List<MetaData> convertedList = metaDataHandler.convert(json);
        assertThat(convertedList, is(metaDataList));
    }

    @Test
    public void testDoRefresh() {
        List<MetaData> metaDataList = createFakeMetaDataObjects(3);
        metaDataHandler.doRefresh(metaDataList);
        subscribers.forEach(subscriber -> verify(subscriber).refresh());
        metaDataList.forEach(metaData ->
                subscribers.forEach(subscriber -> verify(subscriber).onSubscribe(metaData)));
    }

    @Test
    public void testDoUpdate() {
        List<MetaData> metaDataList = createFakeMetaDataObjects(4);
        metaDataHandler.doUpdate(metaDataList);
        metaDataList.forEach(metaData ->
                subscribers.forEach(subscriber -> verify(subscriber).onSubscribe(metaData)));
    }

    @Test
    public void testDoDelete() {
        List<MetaData> metaDataList = createFakeMetaDataObjects(3);
        metaDataHandler.doDelete(metaDataList);
        metaDataList.forEach(metaData ->
                subscribers.forEach(subscriber -> verify(subscriber).unSubscribe(metaData)));
    }

    private List<MetaData> createFakeMetaDataObjects(final int count) {
        List<MetaData> result = new LinkedList<>();
        for (int i = 1; i <= count; i++) {
            MetaData metaData = new MetaData();
            metaData.setAppName("appKey-" + i);
            result.add(metaData);
        }
        return result;
    }
}
