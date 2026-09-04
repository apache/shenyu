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

import io.kubernetes.client.extended.workqueue.WorkQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import org.apache.shenyu.k8s.cache.K8sCacheReadiness;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Readiness must wait for the initial reconciliation backlog to drain in addition to the
 * informer sync, and must observe the queues empty twice in a row to close the race of an
 * item popped but not yet reconciled.
 */
public final class K8sCacheReadinessTest {

    @Test
    public void testNotReadyWhileInformerUnsynced() {
        SharedIndexInformer<?> informer = mock(SharedIndexInformer.class);
        when(informer.hasSynced()).thenReturn(false);
        K8sCacheReadiness readiness = new K8sCacheReadiness(List.of(informer), List.of());

        Assertions.assertFalse(readiness.isReady());
        Assertions.assertEquals(1L, readiness.pendingInformers());
    }

    @Test
    public void testNotReadyWhileWorkQueuesHoldItems() {
        K8sCacheReadiness readiness = readiness(true, 3);

        Assertions.assertFalse(readiness.isReady());
        Assertions.assertEquals(3L, readiness.pendingWorkItems());
    }

    @Test
    public void testReadyOnlyAfterRepeatedDrainedObservations() {
        SharedIndexInformer<?> informer = syncedInformer();
        WorkQueue<?> queue = queue(0);
        K8sCacheReadiness readiness = new K8sCacheReadiness(List.of(informer), List.of(queue));

        Assertions.assertFalse(readiness.isReady(), "a single drained observation must not flip readiness");
        Assertions.assertTrue(readiness.isReady(), "the second consecutive drained observation must");
        // readiness latches: a later refill (e.g. resync) does not flip it back
        when(queue.length()).thenReturn(5);
        Assertions.assertTrue(readiness.isReady());
    }

    @Test
    public void testRefillResetsTheDrainedStreak() {
        SharedIndexInformer<?> informer = syncedInformer();
        WorkQueue<?> queue = queue(0);
        K8sCacheReadiness readiness = new K8sCacheReadiness(List.of(informer), List.of(queue));

        Assertions.assertFalse(readiness.isReady());
        when(queue.length()).thenReturn(2);
        Assertions.assertFalse(readiness.isReady());
        when(queue.length()).thenReturn(0);
        Assertions.assertFalse(readiness.isReady(), "streak was reset by the refill");
        Assertions.assertTrue(readiness.isReady());
    }

    private K8sCacheReadiness readiness(final boolean synced, final int queueLength) {
        return new K8sCacheReadiness(List.of(syncedInformer()), List.of(queue(queueLength)));
    }

    private SharedIndexInformer<?> syncedInformer() {
        SharedIndexInformer<?> informer = mock(SharedIndexInformer.class);
        when(informer.hasSynced()).thenReturn(true);
        return informer;
    }

    private WorkQueue<?> queue(final int length) {
        WorkQueue<?> queue = mock(WorkQueue.class);
        when(queue.length()).thenReturn(length);
        return queue;
    }
}
