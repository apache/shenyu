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

import io.kubernetes.client.informer.SharedIndexInformer;
import org.apache.shenyu.k8s.cache.K8sCacheReadiness;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.List;

/**
 * Test cases for K8sCacheReadiness.
 */
public final class K8sCacheReadinessTest {

    private SharedIndexInformer<?> informer(final boolean synced) {
        SharedIndexInformer<?> informer = Mockito.mock(SharedIndexInformer.class);
        Mockito.when(informer.hasSynced()).thenReturn(synced);
        return informer;
    }

    @Test
    public void testNotReadyWhenAnyInformerUnsynced() {
        K8sCacheReadiness readiness = new K8sCacheReadiness(List.of(informer(true), informer(false)));
        Assertions.assertFalse(readiness.isReady());
        Assertions.assertEquals(1, readiness.pendingInformers());
    }

    @Test
    public void testReadyWhenAllInformersSynced() {
        K8sCacheReadiness readiness = new K8sCacheReadiness(List.of(informer(true), informer(true)));
        Assertions.assertTrue(readiness.isReady());
        Assertions.assertEquals(0, readiness.pendingInformers());
    }

    @Test
    public void testReadinessLatchesOnceSynced() {
        SharedIndexInformer<?> informer = Mockito.mock(SharedIndexInformer.class);
        Mockito.when(informer.hasSynced()).thenReturn(true).thenReturn(false);
        K8sCacheReadiness readiness = new K8sCacheReadiness(List.of(informer));
        Assertions.assertTrue(readiness.isReady());
        Assertions.assertTrue(readiness.isReady(), "readiness must latch and not flip back on re-list");
    }

    @Test
    public void testRejectsEmptyInformers() {
        Assertions.assertThrows(IllegalArgumentException.class, () -> new K8sCacheReadiness(List.of()));
    }
}
