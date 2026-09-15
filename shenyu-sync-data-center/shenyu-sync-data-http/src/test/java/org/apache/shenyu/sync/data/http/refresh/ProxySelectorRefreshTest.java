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

import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for {@link ProxySelectorRefresh}.
 */
public final class ProxySelectorRefreshTest {

    private final StubProxySelectorDataSubscriber subscriber = new StubProxySelectorDataSubscriber();

    private final ProxySelectorRefresh proxySelectorRefresh =
            new ProxySelectorRefresh(Collections.singletonList(subscriber));

    @Test
    public void testRefreshWithEmptyDataShouldClearSubscriberCache() {
        List<ProxySelectorData> empty = Collections.emptyList();
        proxySelectorRefresh.refresh(empty);
        assertTrue(subscriber.refreshed, "empty snapshot must invoke subscriber.refresh() to clear stale data");
        assertFalse(subscriber.subscribed, "empty snapshot must not subscribe data");
    }

    @Test
    public void testRefreshWithDataShouldSubscribe() {
        proxySelectorRefresh.refresh(Collections.singletonList(new ProxySelectorData()));
        assertTrue(subscriber.subscribed);
    }

    private static final class StubProxySelectorDataSubscriber implements ProxySelectorDataSubscriber {

        private boolean refreshed;

        private boolean subscribed;

        @Override
        public void onSubscribe(final ProxySelectorData proxySelectorData) {
            subscribed = true;
        }

        @Override
        public void unSubscribe(final ProxySelectorData proxySelectorData) {
        }

        @Override
        public void refresh() {
            refreshed = true;
        }
    }
}
