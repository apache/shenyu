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

package org.apache.shenyu.client.core.register;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.shenyu.client.core.shutdown.SoulClientShutdownHook;
import org.apache.shenyu.register.client.api.SoulClientRegisterRepository;
import org.apache.shenyu.register.common.config.SoulRegisterCenterConfig;
import org.apache.shenyu.spi.ExtensionLoader;

/**
 * The type Soul client register repository factory.
 */
public final class SoulClientRegisterRepositoryFactory {
    
    private static final Map<String, SoulClientRegisterRepository> REPOSITORY_MAP = new ConcurrentHashMap<>();
    
    /**
     * New instance soul client register repository.
     *
     * @param soulRegisterCenterConfig the soul register center config
     * @return the soul client register repository
     */
    public static SoulClientRegisterRepository newInstance(final SoulRegisterCenterConfig soulRegisterCenterConfig) {
        if (!REPOSITORY_MAP.containsKey(soulRegisterCenterConfig.getRegisterType())) {
            SoulClientRegisterRepository result = ExtensionLoader.getExtensionLoader(SoulClientRegisterRepository.class).getJoin(soulRegisterCenterConfig.getRegisterType());
            result.init(soulRegisterCenterConfig);
            SoulClientShutdownHook.set(result, soulRegisterCenterConfig.getProps());
            REPOSITORY_MAP.put(soulRegisterCenterConfig.getRegisterType(), result);
            return result;
        }
        return REPOSITORY_MAP.get(soulRegisterCenterConfig.getRegisterType());
    }
}
