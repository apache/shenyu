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

package org.dromara.soul.plugin.monitor;

import org.dromara.soul.cache.api.PluginDataSubscriber;
import org.dromara.soul.common.config.MonitorConfig;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.metrics.config.MetricsConfig;
import org.dromara.soul.metrics.facade.MetricsTrackerFacade;

import java.util.Objects;

public class MonitorPluginDataSubscriber implements PluginDataSubscriber {
    
    @Override
    public void onSubscribe(PluginData pluginData) {
        if (Objects.nonNull(pluginData) && pluginData.getEnabled() && PluginEnum.MONITOR.getName().equals(pluginData.getName())) {
            MetricsConfig monitorConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), MetricsConfig.class);
            MetricsTrackerFacade.getInstance().init(monitorConfig);
        }
    }
}
