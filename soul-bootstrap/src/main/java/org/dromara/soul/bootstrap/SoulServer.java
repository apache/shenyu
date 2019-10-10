/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.soul.bootstrap;

import org.dromara.soul.common.extension.ExtensionLoader;
import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.config.api.ConfigLoader;
import org.dromara.soul.config.api.original.ServerConfigLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * SoulServer .
 *
 * @author sixh
 */
public class SoulServer {
    private ExtensionLoader<ConfigLoader> extensionLoader = ExtensionLoader.getExtensionLoader(ConfigLoader.class);
    private Logger logger = LoggerFactory.getLogger(SoulServer.class);

    public void start() {
        loadConfig();
    }

    /**
     * oad configuration information.
     */
    private void loadConfig() {
        ServerConfigLoader loader = new ServerConfigLoader();
        loader.load(ConfigLoader.Context::new, (context, config) -> {
            if (config != null) {
                if (StringUtils.isNotBlank(config.getConfigMode())) {
                    String configMode = config.getConfigMode();
                    ConfigLoader configLoader = extensionLoader.getJoin(configMode);
                    logger.info("Load the configuration【{}】information...", configMode);
                    configLoader.load(context, (context1, config1) -> {
                        logger.info("Configuration information: {}", config1);
                    });
                }
            }
        });
    }
}
