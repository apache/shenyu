/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.fusing.resilience4j;

import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.circuitbreaker.CircuitBreakerRegistry;
import io.vavr.control.Try;
import org.dromara.soul.fusing.api.FusingService;
import org.dromara.soul.fusing.api.config.FusingConfig;
import org.dromara.soul.fusing.resilience4j.builder.CircuitBreakerConfigBuilder;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * The type Resilience 4 j service.
 *
 * @author xiaoyu
 */
public class Resilience4jService implements FusingService {

    @Override
    public Object execute(FusingConfig config, Supplier<Object> execute, Function<? super Throwable, ? extends Object> fallback) {
        CircuitBreakerRegistry circuitBreakerRegistry = CircuitBreakerRegistry.of(CircuitBreakerConfigBuilder.build(config));
        CircuitBreaker circuitBreaker = circuitBreakerRegistry.circuitBreaker(config.getCommandKey());
        Supplier<Object> decoratedSupplier = CircuitBreaker.decorateSupplier(circuitBreaker, execute);
        return Try.ofSupplier(decoratedSupplier).recover(fallback).get();
    }
}
