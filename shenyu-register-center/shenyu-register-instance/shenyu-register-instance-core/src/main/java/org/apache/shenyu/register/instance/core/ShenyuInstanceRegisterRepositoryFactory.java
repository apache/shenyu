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

package org.apache.shenyu.register.instance.core;

import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.register.instance.api.config.RegisterConfig;
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
        if (!REPOSITORY_MAP.containsKey(registerType)) {
            ShenyuInstanceRegisterRepository result = ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class).getJoin(registerType);
            REPOSITORY_MAP.put(registerType, result);
            return result;
        }
        return REPOSITORY_MAP.get(registerType);
    }
    
    /**
     * New and init instance shenyu instance register repository.
     *
     * @param config the config
     * @return the shenyu instance register repository
     */
    public static ShenyuInstanceRegisterRepository newAndInitInstance(final RegisterConfig config) {
        String registerType = config.getRegisterType();
        if (!REPOSITORY_MAP.containsKey(registerType)) {
            ShenyuInstanceRegisterRepository result = ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class).getJoin(registerType);
            result.init(config);
            REPOSITORY_MAP.put(registerType, result);
            return result;
        }
        return REPOSITORY_MAP.get(registerType);
    }
}
