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

import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.config.api.ConfigException;
import org.dromara.soul.config.api.ConfigLoader;

import java.util.function.Supplier;

/**
 * NacosConfigLoader .
 * nacos 配置加载信息.
 * 2019/8/17
 *
 * @author sixh
 */
public class NacosConfigLoader extends ConfigLoader<NacosConfig> {

    public NacosConfigLoader() {
        ConfigEnv.getInstance().putBean(new NacosConfig());
    }

    @Override
    public void load(Supplier<Context> context, LoaderHandler<NacosConfig> handler) {
        LoaderHandler<NacosConfig> nacosHandler = this::nacosLoad;
        againLoad(context, nacosHandler, NacosConfig.class);
    }

    private void nacosLoad(Supplier<Context> context, NacosConfig config) {
        if (config != null) {
            check(config);
            System.out.println("nacos" + config);
            NacosClient client = new NacosClient();
            client.pull(config);
        } else {
            throw new ConfigException("nacos config is null");
        }
    }

    private void check(NacosConfig config) {
        if (StringUtils.isBlank(config.getServer())) {
            throw new ConfigException("nacos.server is null");
        }
        if (StringUtils.isBlank(config.getDataId())) {
            throw new ConfigException("nacos.dataId is null");
        }
        if (StringUtils.isBlank(config.getFileExtension())) {
            throw new ConfigException("nacos.fileExtension is null");
        }
        if (StringUtils.isBlank(config.getGroup())) {
            throw new ConfigException("nacos.group is null");
        }
    }
}
