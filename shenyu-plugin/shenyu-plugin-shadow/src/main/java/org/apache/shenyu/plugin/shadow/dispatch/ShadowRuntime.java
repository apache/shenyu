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

package org.apache.shenyu.plugin.shadow.dispatch;

import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.apache.shenyu.plugin.shadow.config.ShadowSkipReason;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Runtime state for one shadow rule: its validated handle, bounded dispatcher
 * and skip counters.
 */
public class ShadowRuntime {

    private final ShadowHandle handle;

    private final ShadowDispatcher dispatcher;

    private final Map<ShadowSkipReason, AtomicLong> skips = new ConcurrentHashMap<>();

    public ShadowRuntime(final ShadowHandle handle, final ShadowDispatcher dispatcher) {
        this.handle = Objects.requireNonNull(handle);
        this.dispatcher = Objects.requireNonNull(dispatcher);
    }

    /**
     * Gets the validated handle.
     *
     * @return the validated handle
     */
    public ShadowHandle getHandle() {
        return handle;
    }

    /**
     * Mirror one shadow call.
     *
     * @param task the shadow call
     * @return true when the call was queued
     */
    public boolean mirror(final ShadowTask task) {
        return dispatcher.dispatch(task);
    }

    /**
     * Count one skipped sample.
     *
     * @param reason why the sample was skipped
     */
    public void recordSkip(final ShadowSkipReason reason) {
        skips.computeIfAbsent(reason, key -> new AtomicLong()).incrementAndGet();
    }

    /**
     * Gets the number of samples skipped for one reason.
     *
     * @param reason the skip reason
     * @return the observed skip count
     */
    public long skipCount(final ShadowSkipReason reason) {
        AtomicLong counter = skips.get(reason);
        return Objects.isNull(counter) ? 0L : counter.get();
    }

    /**
     * Release the rule's dispatcher resources.
     */
    public void close() {
        dispatcher.close();
    }
}
