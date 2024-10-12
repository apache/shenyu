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

package org.apache.shenyu.registry.core;

import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.spi.ExtensionLoader;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The type Shenyu instance register repository factory.
 */
public final class ShenyuInstanceRegisterRepositoryFactory {
    
    private static final Map<String, ShenyuInstanceRegisterRepository> REPOSITORY_MAP = new ConcurrentHashMap<>();
    
    /**
     * New instance shenyu instance register repository.
     *
     * @param registerType the config
     * @return the shenyu instance register repository
     */
    public static ShenyuInstanceRegisterRepository newInstance(final String registerType) {
        return REPOSITORY_MAP.computeIfAbsent(registerType, ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class)::getJoin);
    }
    
    /**
     * New and init instance shenyu instance register repository.
     *
     * @param config the config
     * @return the shenyu instance register repository
     */
    public static ShenyuInstanceRegisterRepository newAndInitInstance(final RegisterConfig config) {
        return REPOSITORY_MAP.computeIfAbsent(config.getRegisterType(), registerType -> {
            ShenyuInstanceRegisterRepository result = ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class).getJoin(registerType);
            result.init(config);
            return result;
        });
    }

    /**
     * reNew and init instance shenyu instance register repository.
     *
     * @param config the config
     * @return the shenyu instance register repository
     */
    public static ShenyuInstanceRegisterRepository reNewAndInitInstance(final RegisterConfig config) {
        ShenyuInstanceRegisterRepository result = ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class).getJoin(config.getRegisterType());
        result.init(config);
        REPOSITORY_MAP.put(config.getRegisterType(), result);
        return result;
    }
}
