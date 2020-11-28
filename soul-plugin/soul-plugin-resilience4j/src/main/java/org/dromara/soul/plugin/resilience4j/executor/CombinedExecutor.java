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

package org.dromara.soul.plugin.resilience4j.executor;

import io.github.resilience4j.ratelimiter.RateLimiter;
import io.github.resilience4j.reactor.ratelimiter.operator.RateLimiterOperator;
import org.dromara.soul.plugin.resilience4j.factory.ResilienceRegistryFactory;
import reactor.core.publisher.Mono;

import java.util.function.Function;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.reactor.circuitbreaker.operator.CircuitBreakerOperator;
import org.dromara.soul.plugin.resilience4j.conf.ResilienceConf;

/**
 * CombinedExecutor.
 *
 * @author zhanglei
 */
public class CombinedExecutor implements Executor {

    @Override
    public <T> Mono<T> run(final Mono<T> run, final Function<Throwable, Mono<T>> fallback, final ResilienceConf resilienceConf) {
        RateLimiter rateLimiter = ResilienceRegistryFactory.rateLimiter(resilienceConf.getId(), resilienceConf.getRateLimiterConfig());
        CircuitBreaker circuitBreaker = ResilienceRegistryFactory.circuitBreaker(resilienceConf.getId(), resilienceConf.getCircuitBreakerConfig());
        Mono<T> to = run.transformDeferred(CircuitBreakerOperator.of(circuitBreaker))
                .transformDeferred(RateLimiterOperator.of(rateLimiter))
                .timeout(resilienceConf.getTimeLimiterConfig().getTimeoutDuration())
                .doOnError(TimeoutException.class, t -> circuitBreaker.onError(
                        resilienceConf.getTimeLimiterConfig().getTimeoutDuration().toMillis(),
                        TimeUnit.MILLISECONDS,
                        t));
        if (fallback != null) {
            to = to.onErrorResume(fallback);
        }
        return to;
    }
}
