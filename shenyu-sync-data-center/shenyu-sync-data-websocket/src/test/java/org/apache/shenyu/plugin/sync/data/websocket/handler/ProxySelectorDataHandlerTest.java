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
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * Test cases for {@link ProxySelectorDataHandler}.
 */
public final class ProxySelectorDataHandlerTest {

    private final List<ProxySelectorDataSubscriber> subscribers;

    private final ProxySelectorDataHandler dataHandler;

    public ProxySelectorDataHandlerTest() {
        subscribers = new LinkedList<>();
        subscribers.add(mock(ProxySelectorDataSubscriber.class));
        subscribers.add(mock(ProxySelectorDataSubscriber.class));
        dataHandler = new ProxySelectorDataHandler(subscribers);
    }

    @Test
    public void testConvert() {
        ProxySelectorData proxySelectorData = proxySelectorData("selector-1", 20000);
        List<ProxySelectorData> sources = Collections.singletonList(proxySelectorData);

        List<ProxySelectorData> result = dataHandler.convert(new Gson().toJson(sources));

        assertEquals(1, result.size());
        assertEquals("selector-1", result.get(0).getName());
        assertEquals(20000, result.get(0).getForwardPort());
    }

    @Test
    public void testDoRefresh() {
        List<ProxySelectorData> dataList = createFakeProxySelectorData(3);

        dataHandler.doRefresh(dataList);

        subscribers.forEach(subscriber -> verify(subscriber).refresh());
        dataList.forEach(data ->
                subscribers.forEach(subscriber -> verify(subscriber).onSubscribe(data)));
    }

    @Test
    public void testDoUpdate() {
        List<ProxySelectorData> dataList = createFakeProxySelectorData(4);

        dataHandler.doUpdate(dataList);

        dataList.forEach(data ->
                subscribers.forEach(subscriber -> verify(subscriber).onSubscribe(data)));
    }

    @Test
    public void testDoDelete() {
        List<ProxySelectorData> dataList = createFakeProxySelectorData(3);

        dataHandler.doDelete(dataList);

        dataList.forEach(data ->
                subscribers.forEach(subscriber -> verify(subscriber).unSubscribe(data)));
    }

    @Test
    public void testDoDeleteShouldTolerateNullItems() {
        ProxySelectorData proxySelectorData = proxySelectorData("selector-1", 20000);

        dataHandler.doDelete(Arrays.asList(proxySelectorData, null));

        subscribers.forEach(subscriber -> verify(subscriber).unSubscribe(proxySelectorData));
    }

    private List<ProxySelectorData> createFakeProxySelectorData(final int count) {
        List<ProxySelectorData> result = new LinkedList<>();
        for (int i = 1; i <= count; i++) {
            result.add(proxySelectorData("selector-" + i, 20000 + i));
        }
        return result;
    }

    private ProxySelectorData proxySelectorData(final String name, final int forwardPort) {
        ProxySelectorData proxySelectorData = new ProxySelectorData();
        proxySelectorData.setName(name);
        proxySelectorData.setForwardPort(forwardPort);
        return proxySelectorData;
    }
}
