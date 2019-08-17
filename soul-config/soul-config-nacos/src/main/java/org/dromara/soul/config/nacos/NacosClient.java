/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.dromara.soul.config.nacos;

import com.alibaba.nacos.api.NacosFactory;
import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.exception.NacosException;
import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.config.api.ConfigException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Properties;

/**
 * NacosClient .
 * nacos client pull config content.
 * 2019/8/17
 *
 * @author sixh
 */
public class NacosClient {
    private static final Logger logger = LoggerFactory.getLogger(NacosClient.class);
    private static final String NACOS_SERVER_ADDR_KEY = "serverAddr";

    /**
     * 拉取nacos的配置信息.
     *
     * @param config nacos.
     */
    InputStream pull(NacosConfig config) {
        Properties properties = new Properties();
        properties.put(NACOS_SERVER_ADDR_KEY, config.getServer());
        try {
            ConfigService configService = NacosFactory.createConfigService(properties);
            String content = configService.getConfig(config.getDataId(), config.getGroup(), 5000);
            if (logger.isDebugEnabled()) {
                logger.debug("nacos content {}", content);
            }
            if (StringUtils.isBlank(content)) {
                return null;
            }
            return new ByteArrayInputStream(content.getBytes());
        } catch (NacosException e) {
            throw new ConfigException(e);
        }
    }
}
