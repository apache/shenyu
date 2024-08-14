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

import org.apache.shenyu.client.core.shutdown.ShenyuClientShutdownHook;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.spi.ExtensionLoader;

/**
 * The type shenyu client register repository factory.
 */
public final class ShenyuClientRegisterRepositoryFactory {
    
    private static final Map<String, ShenyuClientRegisterRepository> REPOSITORY_MAP = new ConcurrentHashMap<>();

    /**
     * Retrieves the repository map containing mappings of register types to corresponding ShenyuClientRegisterRepository instances.
     *
     * @return A {@link Map} where keys are register types and values are associated {@link ShenyuClientRegisterRepository} instances.
     */
    public static Map<String, ShenyuClientRegisterRepository> getRepositoryMap() {
        return REPOSITORY_MAP;
    }
    
    /**
     * New instance shenyu client register repository.
     *
     * @param shenyuRegisterCenterConfig the shenyu register center config
     * @return the shenyu client register repository
     */
    public static ShenyuClientRegisterRepository newInstance(final ShenyuRegisterCenterConfig shenyuRegisterCenterConfig) {
        if (!REPOSITORY_MAP.containsKey(shenyuRegisterCenterConfig.getRegisterType())) {
            ShenyuClientRegisterRepository result = ExtensionLoader.getExtensionLoader(ShenyuClientRegisterRepository.class).getJoin(shenyuRegisterCenterConfig.getRegisterType());
            result.init(shenyuRegisterCenterConfig);
            ShenyuClientShutdownHook.set(result, shenyuRegisterCenterConfig.getProps());
            REPOSITORY_MAP.put(shenyuRegisterCenterConfig.getRegisterType(), result);
            return result;
        }
        return REPOSITORY_MAP.get(shenyuRegisterCenterConfig.getRegisterType());
    }
}
