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

package org.apache.shenyu.plugin.tars.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.TarsRegisterConfig;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.tars.cache.ApplicationConfigCache;

import java.util.Objects;

/**
 * The type tars plugin data handler.
 */
public class TarsPluginDataHandler implements PluginDataHandler {

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.nonNull(pluginData) && Boolean.TRUE.equals(pluginData.getEnabled())) {
            TarsRegisterConfig tarsRegisterConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), TarsRegisterConfig.class);
            TarsRegisterConfig exist = Singleton.INST.get(TarsRegisterConfig.class);
            if (Objects.isNull(tarsRegisterConfig)) {
                return;
            }
            if (Objects.isNull(exist) || !tarsRegisterConfig.equals(exist)) {
                // If it is null, cache it
                ApplicationConfigCache.getInstance().init(tarsRegisterConfig);
            }
            Singleton.INST.single(TarsRegisterConfig.class, tarsRegisterConfig);
        }
    }
    
    @Override
    public String pluginNamed() {
        return PluginEnum.TARS.getName();
    }
    
    @Override
    public void handlerSelector(final SelectorData selectorData) {
        if (Objects.isNull(selectorData.getName())) {
            return;
        }
        ApplicationConfigCache.getInstance().initPrxClass(selectorData);
    }
    
    @Override
    public void removeSelector(final SelectorData selectorData) {
        if (Objects.isNull(selectorData.getName())) {
            return;
        }
        ApplicationConfigCache.getInstance().invalidate(selectorData.getName());
    }
}
