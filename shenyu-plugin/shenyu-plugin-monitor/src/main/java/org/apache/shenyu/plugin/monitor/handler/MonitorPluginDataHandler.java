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

package org.apache.shenyu.plugin.monitor.handler;

import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.metrics.config.MetricsConfig;
import org.apache.shenyu.metrics.facade.MetricsTrackerFacade;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.common.utils.Singleton;

/**
 * The type Monitor plugin data handler.
 */
public class MonitorPluginDataHandler implements PluginDataHandler {
    
    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.nonNull(pluginData) && pluginData.getEnabled()) {
            MetricsConfig monitorConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), MetricsConfig.class);
            if (!checkConfig(monitorConfig)) {
                return;
            }
            if (!MetricsTrackerFacade.getInstance().isStarted()) {
                start(monitorConfig);
            } else if (!monitorConfig.equals(Singleton.INST.get(MetricsConfig.class))) {
                restart(monitorConfig);
            }
        } else {
            stop();
        }
    }
    
    @Override
    public String pluginNamed() {
        return PluginEnum.MONITOR.getName();
    }
    
    private boolean checkConfig(final MetricsConfig monitorConfig) {
        return Objects.nonNull(monitorConfig)
                && StringUtils.isNoneBlank(monitorConfig.getHost())
                && Objects.nonNull(monitorConfig.getPort())
                && Objects.nonNull(monitorConfig.getAsync());
    }
    
    private void restart(final MetricsConfig monitorConfig) {
        stop();
        start(monitorConfig);
    }
    
    private void start(final MetricsConfig monitorConfig) {
        MetricsTrackerFacade.getInstance().start(monitorConfig);
        Singleton.INST.single(MetricsConfig.class, monitorConfig);
    }
    
    private void stop() {
        MetricsTrackerFacade.getInstance().stop();
    }
}
