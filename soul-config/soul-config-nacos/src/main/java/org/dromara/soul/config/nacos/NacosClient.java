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
import org.dromara.soul.config.api.ConfigException;

import java.util.Properties;

/**
 * NacosClient .
 * 2019/8/17
 *
 * @author sixh
 */
public class NacosClient {

    private static final String NACOS_SERVER_ADDR_KEY = "serverAddr";
    private static final String NACOS_DATA_ID_KEY = "dataId";
    private static final String NACOS_GROUP_KEY = "group";

    /**
     * 拉取nacos的配置信息.
     *
     * @param config nacos.
     */
    public void pull(NacosConfig config) {
        Properties properties = new Properties();
        properties.put(NACOS_SERVER_ADDR_KEY, config.getServer());
        try {
            ConfigService configService = NacosFactory.createConfigService(properties);
            String config1 = configService.getConfig(config.getDataId(), config.getGroup(), 5000);
            System.out.println(config1);
        } catch (NacosException e) {
            throw new ConfigException(e);
        }
    }
}
