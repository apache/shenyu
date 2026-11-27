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
import org.mockito.InOrder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

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

    @Test
    public void testNewSnapshotUnsubscribesOnlyDeletedMetadata() {
        MetaDataSubscriber subscriber = mock(MetaDataSubscriber.class);
        MetaDataRefresh refresh = new MetaDataRefresh(List.of(subscriber));
        MetaData deleted = metadata("old-id", "/old");
        MetaData retained = metadata("retained-id", "/retained");

        assertTrue(refresh.refresh(snapshot("first", 1, List.of(deleted, retained))));
        assertTrue(refresh.refresh(snapshot("second", 2, List.of(retained))));
        assertFalse(refresh.refresh(snapshot("late", 1, List.of(deleted, retained))));

        InOrder calls = inOrder(subscriber);
        calls.verify(subscriber).onSubscribe(deleted);
        calls.verify(subscriber).onSubscribe(retained);
        calls.verify(subscriber).unSubscribe(deleted);
        calls.verify(subscriber).onSubscribe(retained);
        calls.verifyNoMoreInteractions();
        verify(subscriber, never()).refresh();
    }

    @Test
    public void testNullRowDoesNotInterruptSnapshotReconciliation() {
        MetaDataSubscriber subscriber = mock(MetaDataSubscriber.class);
        MetaDataRefresh refresh = new MetaDataRefresh(List.of(subscriber));
        MetaData deleted = metadata("old-id", "/old");
        MetaData retained = metadata("retained-id", "/retained");

        assertTrue(refresh.refresh(snapshot("first", 1, List.of(deleted, retained))));
        assertTrue(refresh.refresh(snapshot("with-null", 2, Arrays.asList(null, retained))));
        assertTrue(refresh.refresh(snapshot("empty", 3, List.of())));

        InOrder calls = inOrder(subscriber);
        calls.verify(subscriber).onSubscribe(deleted);
        calls.verify(subscriber).onSubscribe(retained);
        calls.verify(subscriber).unSubscribe(deleted);
        calls.verify(subscriber).onSubscribe(retained);
        calls.verify(subscriber).unSubscribe(retained);
        calls.verify(subscriber).refresh();
        calls.verifyNoMoreInteractions();
    }

    @Test
    public void testFailedSubscriberAllowsReplayOfTheSameMetadataSnapshot() {
        MetaDataSubscriber subscriber = mock(MetaDataSubscriber.class);
        MetaDataRefresh refresh = new MetaDataRefresh(List.of(subscriber));
        MetaData retained = metadata("retained-id", "/retained");
        MetaData rejected = metadata("rejected-id", "/rejected");
        assertTrue(refresh.refresh(snapshot("first", 1, List.of(retained))));
        doThrow(new IllegalStateException("temporary subscriber failure")).doNothing().when(subscriber).onSubscribe(rejected);

        JsonObject second = snapshot("second", 2, List.of(retained, rejected));
        assertThrows(IllegalStateException.class, () -> refresh.refresh(second));
        String digestAfterFailure = refresh.cacheConfigData().getMd5();
        assertTrue(refresh.refresh(second), "an identical metadata snapshot must be retried after a subscriber failed");
        assertThat(digestAfterFailure, is("first"));
        assertThat(refresh.cacheConfigData().getMd5(), is("second"));
    }

    @Test
    public void testSupersedingSnapshotRemovesPartiallyAppliedMetadata() {
        MetaDataSubscriber first = mock(MetaDataSubscriber.class);
        MetaDataSubscriber second = mock(MetaDataSubscriber.class);
        MetaDataRefresh refresh = new MetaDataRefresh(List.of(first, second));
        MetaData retained = metadata("retained", "/kept");
        MetaData partial = metadata("partial", "/temporary");
        assertTrue(refresh.refresh(snapshot("original", 1, List.of(retained))));
        doAnswer(invocation -> {
            assertThat(refresh.cacheConfigData().getMd5(), is("original"));
            throw new IllegalStateException("second subscriber failed");
        }).when(second).onSubscribe(partial);

        assertThrows(IllegalStateException.class, () -> refresh.refresh(snapshot("intermediate", 2, List.of(retained, partial))));
        verify(first).onSubscribe(partial);
        assertTrue(refresh.refresh(snapshot("original", 3, List.of(retained))));
        verify(first).unSubscribe(partial);
        verify(second).unSubscribe(partial);
        assertFalse(refresh.refresh(snapshot("intermediate", 2, List.of(retained, partial))));
    }

    @Test
    public void testFailedUnsubscribeIsRetriedBeforeConfirmingAnEmptySnapshot() {
        MetaDataSubscriber subscriber = mock(MetaDataSubscriber.class);
        MetaDataRefresh refresh = new MetaDataRefresh(List.of(subscriber));
        MetaData deleted = metadata("deleted", "/deleted");
        assertTrue(refresh.refresh(snapshot("initial", 1, List.of(deleted))));
        doThrow(new IllegalStateException("temporary removal failure")).doNothing().when(subscriber).unSubscribe(deleted);
        JsonObject empty = snapshot("empty", 2, List.of());

        assertThrows(IllegalStateException.class, () -> refresh.refresh(empty));
        verify(subscriber, never()).refresh();
        assertTrue(refresh.refresh(empty));
        verify(subscriber).refresh();
        assertThat(refresh.cacheConfigData().getMd5(), is("empty"));
        assertFalse(refresh.refresh(empty));
    }

    @Test
    public void testSnapshotWithOnlyNullRowsDoesNotAdvanceVersion() {
        MetaDataSubscriber subscriber = mock(MetaDataSubscriber.class);
        MetaDataRefresh refresh = new MetaDataRefresh(List.of(subscriber));
        MetaData retained = metadata("retained-id", "/retained");

        assertTrue(refresh.refresh(snapshot("first", 1, List.of(retained))));
        boolean updated = refresh.refresh(snapshot("malformed", 3, Collections.singletonList(null)));
        assertThat(refresh.cacheConfigData().getLastModifyTime(), is(1L));
        assertThat(refresh.cacheConfigData().getMd5(), is("first"));
        assertFalse(updated);
        verify(subscriber, never()).unSubscribe(retained);
        verify(subscriber, never()).refresh();

        assertTrue(refresh.refresh(snapshot("empty", 2, List.of())));
        assertThat(refresh.cacheConfigData().getLastModifyTime(), is(2L));
        verify(subscriber).unSubscribe(retained);
        verify(subscriber).refresh();
    }

    @Test
    public void testEmptySnapshotUnsubscribesTheLastMetadataBeforeRefresh() {
        MetaDataSubscriber subscriber = mock(MetaDataSubscriber.class);
        MetaDataRefresh refresh = new MetaDataRefresh(List.of(subscriber));
        MetaData deleted = metadata("old-id", "/old");

        assertTrue(refresh.refresh(snapshot("first", 1, List.of(deleted))));
        assertTrue(refresh.refresh(snapshot("empty", 2, List.of())));
        assertFalse(refresh.refresh(snapshot("empty", 2, List.of())));

        InOrder calls = inOrder(subscriber);
        calls.verify(subscriber).onSubscribe(deleted);
        calls.verify(subscriber).unSubscribe(deleted);
        calls.verify(subscriber).refresh();
        calls.verifyNoMoreInteractions();
    }

    private static MetaData metadata(final String id, final String path) {
        return MetaData.builder().id(id).path(path).rpcType("dubbo").enabled(true).build();
    }

    private static JsonObject snapshot(final String md5, final long revision, final List<MetaData> data) {
        JsonObject json = new JsonObject();
        json.add(ConfigGroupEnum.META_DATA.name(), GsonUtils.getGson().toJsonTree(new ConfigData<>(md5, revision, data)));
        return json;
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
