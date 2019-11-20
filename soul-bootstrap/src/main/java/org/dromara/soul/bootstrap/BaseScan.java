/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.bootstrap;

import java.util.Collection;
import java.util.Map;
import org.dromara.soul.common.extension.ExtensionLoader;
import org.dromara.soul.config.api.Config;
import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.config.api.ConfigException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * ConfigScan
 * Scan configuration information to add.
 *
 * @author sixh
 */
final class BaseScan {

    private Logger logger = LoggerFactory.getLogger(BaseScan.class);
    private ExtensionLoader<Config> extensionLoader = ExtensionLoader.getExtensionLoader(Config.class);

    void scan() {
        Collection<Class<?>> classes = scanSpi();
        for (Class<?> clazz : classes) {
            try {
                ConfigEnv.getInstance().addConfigClass(clazz);
            } catch (ConfigException e) {
                logger.warn("scan config error {}", clazz.getName(), e);
            }
        }
    }

    private <T> Collection<Class<?>> scanSpi() {

        Map<String, Class<?>> joins = extensionLoader.getExtensionClasses();
        return joins.values();
    }
}
