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

package org.apache.shenyu.common.utils;

import org.junit.jupiter.api.Test;

import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class InitialSyncApplicationTest {

    @Test
    void testNestedApplicationAndContextCleanup() {
        CompletableFuture<Void> deferred = new CompletableFuture<>();
        CompletableFuture<Void> result = InitialSyncApplication.run(() -> {
            assertTrue(InitialSyncApplication.isActive());
            InitialSyncApplication.run(() -> InitialSyncApplication.register(deferred));
            assertTrue(InitialSyncApplication.isActive());
        });
        assertFalse(InitialSyncApplication.isActive());
        assertFalse(result.isDone());
        deferred.complete(null);
        assertTrue(result.isDone());
        assertFalse(result.isCompletedExceptionally());
    }

    @Test
    void testFailureDoesNotLeakContext() {
        assertThrows(IllegalStateException.class, () -> InitialSyncApplication.run(() -> {
            throw new IllegalStateException("failed callback");
        }));
        assertFalse(InitialSyncApplication.isActive());
        CompletableFuture<Void> deferred = new CompletableFuture<>();
        CompletableFuture<Void> result = InitialSyncApplication.run(() -> InitialSyncApplication.register(deferred));
        deferred.completeExceptionally(new IllegalStateException("failed application"));
        assertTrue(result.isCompletedExceptionally());
    }
}
