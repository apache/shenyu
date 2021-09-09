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

package org.apache.shenyu.plugin.sofa.handler;

import org.apache.shenyu.common.config.SofaRegisterConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.sofa.cache.ApplicationConfigCache;

import java.util.Objects;

/**
 * The type sofa plugin data handler.
 */
public class SofaPluginDataHandler implements PluginDataHandler {

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (null != pluginData && pluginData.getEnabled()) {
            SofaRegisterConfig sofaRegisterConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), SofaRegisterConfig.class);
            SofaRegisterConfig exist = Singleton.INST.get(SofaRegisterConfig.class);
            if (Objects.isNull(sofaRegisterConfig)) {
                return;
            }
            if (Objects.isNull(exist) || !sofaRegisterConfig.equals(exist)) {
                // If it is null, initialize it
                ApplicationConfigCache.getInstance().init(sofaRegisterConfig);
                ApplicationConfigCache.getInstance().invalidateAll();
            }
            Singleton.INST.single(SofaRegisterConfig.class, sofaRegisterConfig);
        }
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.SOFA.getName();
    }
}
