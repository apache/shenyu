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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.spi.ExtensionLoader;

import java.util.Map;
import java.util.Objects;
import java.util.TreeSet;
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
        return newInstance(registerType, ExtensionLoader.class.getClassLoader());
    }

    // Explicit loader overloads let tests select their SPI resources without warming another loader's cache.
    static ShenyuInstanceRegisterRepository newInstance(final String registerType, final ClassLoader classLoader) {
        checkRegisterType(registerType);
        return REPOSITORY_MAP.computeIfAbsent(registerType, type -> loadRepository(type, classLoader));
    }

    /**
     * Load a repository through SPI without using the factory's per-type cache.
     * The provider's {@code @Join} singleton setting still applies.
     *
     * @param registerType the configured registry type
     * @return the repository
     */
    public static ShenyuInstanceRegisterRepository loadRepository(final String registerType) {
        return loadRepository(registerType, ExtensionLoader.class.getClassLoader());
    }

    static ShenyuInstanceRegisterRepository loadRepository(final String registerType, final ClassLoader classLoader) {
        checkRegisterType(registerType);
        ExtensionLoader<ShenyuInstanceRegisterRepository> loader = ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class, classLoader);
        Map<String, Class<?>> providers = loader.getExtensionClasses1();
        if (!providers.containsKey(registerType)) {
            throw new IllegalArgumentException("No registry implementation found for registerType '" + registerType
                    + "'. Add the matching shenyu-registry-" + registerType
                    + " dependency (or an external registry SPI provider) to the runtime classpath. Available types: "
                    + new TreeSet<>(providers.keySet()));
        }
        return loader.getJoin(registerType);
    }
    
    /**
     * New and init instance shenyu instance register repository.
     *
     * @param config the config
     * @return the shenyu instance register repository
     */
    public static ShenyuInstanceRegisterRepository newAndInitInstance(final RegisterConfig config) {
        return newAndInitInstance(config, ExtensionLoader.class.getClassLoader());
    }

    static ShenyuInstanceRegisterRepository newAndInitInstance(final RegisterConfig config, final ClassLoader classLoader) {
        Objects.requireNonNull(config, "register config is required");
        checkRegisterType(config.getRegisterType());
        return REPOSITORY_MAP.computeIfAbsent(config.getRegisterType(), registerType -> {
            ShenyuInstanceRegisterRepository result = loadRepository(registerType, classLoader);
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
        return reNewAndInitInstance(config, ExtensionLoader.class.getClassLoader());
    }

    static ShenyuInstanceRegisterRepository reNewAndInitInstance(final RegisterConfig config, final ClassLoader classLoader) {
        Objects.requireNonNull(config, "register config is required");
        ShenyuInstanceRegisterRepository result = loadRepository(config.getRegisterType(), classLoader);
        result.init(config);
        REPOSITORY_MAP.put(config.getRegisterType(), result);
        return result;
    }

    private static void checkRegisterType(final String registerType) {
        if (StringUtils.isBlank(registerType)) {
            throw new IllegalArgumentException("registerType is required; configure a registry type and add its SPI provider to the runtime classpath");
        }
    }
}
