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
import org.apache.shenyu.plugin.shadow.report.ShadowReporter;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Owns the shadow runtime of every configured rule.
 *
 * <p>A rule update replaces its runtime and releases the previous dispatcher,
 * so a removed or re-bound rule cannot keep queued work or connections alive.</p>
 */
public class ShadowRuntimeRegistry implements AutoCloseable {

    private final WebClient webClient;

    private final ShadowReporter reporter;

    private final ConcurrentHashMap<String, ShadowRuntime> runtimes = new ConcurrentHashMap<>();

    public ShadowRuntimeRegistry(final WebClient webClient, final ShadowReporter reporter) {
        this.webClient = Objects.requireNonNull(webClient);
        this.reporter = Objects.requireNonNull(reporter);
    }

    /**
     * Register or replace the runtime of one rule.
     *
     * @param key the rule cache key
     * @param handle the validated handle
     */
    public void register(final String key, final ShadowHandle handle) {
        ShadowRuntime previous = runtimes.remove(key);
        if (Objects.nonNull(previous)) {
            previous.close();
        }
        if (Boolean.TRUE.equals(handle.getEnabled())) {
            runtimes.put(key, new ShadowRuntime(handle, new ShadowDispatcher(webClient, handle, key, reporter)));
        }
    }

    /**
     * Remove one rule runtime.
     *
     * @param key the rule cache key
     */
    public void remove(final String key) {
        ShadowRuntime previous = runtimes.remove(key);
        if (Objects.nonNull(previous)) {
            previous.close();
        }
    }

    /**
     * Find the runtime of one rule.
     *
     * @param key the rule cache key
     * @return the runtime, or null when the rule is not shadowed
     */
    public ShadowRuntime find(final String key) {
        return runtimes.get(key);
    }

    @Override
    public void close() {
        runtimes.values().forEach(ShadowRuntime::close);
        runtimes.clear();
    }
}
