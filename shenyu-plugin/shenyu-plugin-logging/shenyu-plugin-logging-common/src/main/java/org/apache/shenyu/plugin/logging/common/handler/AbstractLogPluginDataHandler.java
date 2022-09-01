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

package org.apache.shenyu.plugin.logging.common.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Objects;

/**
 * AbstractLogPluginDataHandler.
 */
public abstract class AbstractLogPluginDataHandler<T> implements PluginDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractLogPluginDataHandler.class);

    /**
     * LogCollector.
     *
     * @return LogCollector
     */
    protected abstract LogCollector logCollector();

    /**
     * LogCollector.
     *
     * @param globalLogConfig globalLogConfig
     * @return LogCollector
     */
    protected abstract void doRefreshConfig(T globalLogConfig);

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        ParameterizedType parameterizedType = (ParameterizedType) this.getClass().getGenericSuperclass();
        Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
        final Class<T> globalLogConfigClass = (Class<T>) actualTypeArguments[0];
        LOG.info("{} handlerPlugin: {}", this.getClass().getSimpleName(), GsonUtils.getGson().toJson(pluginData));
        if (Objects.nonNull(pluginData) && Boolean.TRUE.equals(pluginData.getEnabled())) {
            T globalLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), globalLogConfigClass);
            T exist = Singleton.INST.get(globalLogConfigClass);
            if (Objects.isNull(globalLogConfig)) {
                return;
            }
            if (Objects.isNull(exist) || !globalLogConfig.equals(exist)) {
                // no data, init client
                this.doRefreshConfig(globalLogConfig);
                logCollector().start();
            }
            Singleton.INST.single(globalLogConfigClass, globalLogConfig);
        } else {
            try {
                logCollector().close();
            } catch (Exception e) {
                LOG.error("{} close log collector error", this.getClass().getSimpleName(), e);
            }
        }
    }
}
