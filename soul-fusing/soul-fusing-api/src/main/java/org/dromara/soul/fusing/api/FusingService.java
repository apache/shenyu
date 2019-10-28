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

package org.dromara.soul.fusing.api;

import org.dromara.soul.common.extension.SPI;
import org.dromara.soul.fusing.api.config.FusingConfig;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * The interface Fusing service.
 *
 * @author xiaoyu
 */
@FunctionalInterface
@SPI("hystrix")
public interface FusingService {

    /**
     * Execute object.
     *
     * @param config   the config
     * @param execute  the execute
     * @param fallback the fallback
     * @return the object
     */
    Object execute(FusingConfig config, Supplier<Object> execute, Function<? super Throwable, ? extends Object> fallback);
}
