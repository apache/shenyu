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

package org.apache.shenyu.discovery.api.listener;

import org.junit.Test;
import org.junit.jupiter.api.Assertions;

import java.util.concurrent.atomic.AtomicReference;

import static org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent.Event.ADDED;
import static org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent.Event.DELETED;
import static org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent.Event.IGNORED;
import static org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent.Event.UPDATED;

/**
 * The Test Case For {@link DataChangedEventListener}.
 */
public class DataChangedEventListenerTest {

    @Test
    public void testOnChange() {
        AtomicReference<String> eventResult = new AtomicReference<>("");

        DataChangedEventListener dataChangedEventListener = event -> {
            DiscoveryDataChangedEvent.Event currentEvent = event.getEvent();
            switch (currentEvent) {
                case ADDED:
                    // added logic...
                    eventResult.set(ADDED.name());
                    break;
                case UPDATED:
                    // updated logic...
                    eventResult.set(UPDATED.name());
                    break;
                case DELETED:
                    // deleted logic...
                    eventResult.set(DELETED.name());
                    break;
                case IGNORED:
                    // ignored logic...
                    eventResult.set(IGNORED.name());
                    break;
                default:
                    break;
            }
        };
        DiscoveryDataChangedEvent dataChangedEvent = new DiscoveryDataChangedEvent("key", "value", IGNORED);
        dataChangedEventListener.onChange(dataChangedEvent);

        Assertions.assertEquals(eventResult.get(), IGNORED.name());
    }
}
