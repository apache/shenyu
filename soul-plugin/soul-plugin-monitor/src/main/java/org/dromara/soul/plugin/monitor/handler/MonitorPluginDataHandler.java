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

package org.dromara.soul.plugin.monitor.handler;

import java.util.Objects;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.metrics.config.MetricsConfig;
import org.dromara.soul.metrics.facade.MetricsTrackerFacade;
import org.dromara.soul.plugin.base.handler.PluginDataHandler;

/**
 * The type Monitor plugin data handler.
 *
 * @author xiaoyu
 */
public class MonitorPluginDataHandler implements PluginDataHandler {
    
    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.nonNull(pluginData) && pluginData.getEnabled()) {
            MetricsConfig monitorConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), MetricsConfig.class);
            MetricsTrackerFacade.getInstance().init(monitorConfig);
        } else {
            MetricsTrackerFacade.getInstance().stop();
        }
    }
    
    @Override
    public String pluginNamed() {
        return PluginEnum.MONITOR.getName();
    }
}
