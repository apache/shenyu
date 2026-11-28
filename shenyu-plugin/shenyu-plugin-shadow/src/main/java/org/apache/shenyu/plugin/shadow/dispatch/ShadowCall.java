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

import org.apache.shenyu.plugin.shadow.compare.ShadowObservation;

import java.util.Objects;

/**
 * One queued shadow call: the immutable request plus the primary observation
 * it will be compared with.
 */
public final class ShadowCall {

    private final ShadowTask task;

    private final ShadowObservation primary;

    public ShadowCall(final ShadowTask task, final ShadowObservation primary) {
        this.task = Objects.requireNonNull(task);
        this.primary = Objects.requireNonNull(primary);
    }

    /**
     * Gets the shadow request.
     *
     * @return the shadow request
     */
    public ShadowTask getTask() {
        return task;
    }

    /**
     * Gets the primary observation.
     *
     * @return the primary observation
     */
    public ShadowObservation getPrimary() {
        return primary;
    }
}
