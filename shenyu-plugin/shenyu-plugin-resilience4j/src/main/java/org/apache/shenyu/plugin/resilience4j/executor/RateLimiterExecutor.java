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

package org.apache.shenyu.plugin.resilience4j.executor;

import java.util.Optional;
import java.util.function.Function;

import org.apache.shenyu.plugin.resilience4j.factory.Resilience4JRegistryFactory;
import reactor.core.publisher.Mono;
import io.github.resilience4j.ratelimiter.RateLimiter;
import org.apache.shenyu.plugin.resilience4j.conf.Resilience4JConf;
import io.github.resilience4j.reactor.ratelimiter.operator.RateLimiterOperator;

/**
 * Rate limiter executor.
 */
public class RateLimiterExecutor implements Executor {

    @Override
    public <T> Mono<T> run(final Mono<T> toRun, final Function<Throwable, Mono<T>> fallback, final Resilience4JConf conf) {
        RateLimiter rateLimiter = Resilience4JRegistryFactory.rateLimiter(conf.getId(), conf.getRateLimiterConfig());
        Mono<T> to = toRun.transformDeferred(RateLimiterOperator.of(rateLimiter));

        return Optional.ofNullable(fallback)
                .map(to::onErrorResume)
                .orElse(to);
    }
}
