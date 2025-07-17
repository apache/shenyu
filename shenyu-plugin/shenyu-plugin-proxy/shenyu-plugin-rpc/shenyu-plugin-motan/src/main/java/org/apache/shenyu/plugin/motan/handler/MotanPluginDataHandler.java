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

package org.apache.shenyu.plugin.motan.handler;

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.MotanRegisterConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.convert.selector.MotanUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.motan.cache.ApplicationConfigCache;

import java.util.Objects;

/**
 * The type motan plugin data handler.
 */
public class MotanPluginDataHandler implements PluginDataHandler {

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.nonNull(pluginData) && Boolean.TRUE.equals(pluginData.getEnabled())) {
            MotanRegisterConfig motanRegisterConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), MotanRegisterConfig.class);
            MotanRegisterConfig exist = Singleton.INST.get(MotanRegisterConfig.class);
            if (Objects.isNull(motanRegisterConfig)) {
                return;
            }
            if (Objects.isNull(exist) || !motanRegisterConfig.equals(exist)) {
                // If it is null, initialize it
                ApplicationConfigCache.getInstance().init(motanRegisterConfig);
                ApplicationConfigCache.getInstance().invalidateAll();
            }
            Singleton.INST.single(MotanRegisterConfig.class, motanRegisterConfig);
        }
    }

    @Override
    public void removePlugin(final PluginData pluginData) {
        ApplicationConfigCache.getInstance().invalidateAll();
    }

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        MotanUpstream motanUpstream = GsonUtils.getInstance().fromJson(selectorData.getHandle(), MotanUpstream.class);
        if (Objects.equals(motanUpstream, ApplicationConfigCache
                .getInstance().getUpstream(selectorData.getId()))) {
            return;
        }
        ApplicationConfigCache.getInstance().invalidateWithSelectorId(selectorData.getId());
        ApplicationConfigCache.getInstance().setUpstream(selectorData.getId(), motanUpstream);
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        ApplicationConfigCache.getInstance().invalidateWithSelectorId(selectorData.getId());
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.MOTAN.getName();
    }
}
