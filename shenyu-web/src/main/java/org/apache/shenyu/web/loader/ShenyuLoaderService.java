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

package org.apache.shenyu.web.loader;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.config.ShenyuConfig.ExtPlugin;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * The type Shenyu loader service.
 */
public class ShenyuLoaderService {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuLoaderService.class);
    
    private final ShenyuWebHandler webHandler;
    
    private final CommonPluginDataSubscriber subscriber;
    
    private final ShenyuConfig shenyuConfig;
    
    /**
     * Instantiates a new Shenyu loader service.
     *
     * @param webHandler   the web handler
     * @param subscriber   the subscriber
     * @param shenyuConfig the shenyu config
     */
    public ShenyuLoaderService(final ShenyuWebHandler webHandler, final CommonPluginDataSubscriber subscriber, final ShenyuConfig shenyuConfig) {
        this.subscriber = subscriber;
        this.webHandler = webHandler;
        this.shenyuConfig = shenyuConfig;
        ExtPlugin config = shenyuConfig.getExtPlugin();
        if (config.getEnabled()) {
            ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(config.getThreads(), ShenyuThreadFactory.create("plugin-ext-loader", true));
            executor.scheduleAtFixedRate(this::loaderExtPlugins, config.getScheduleDelay(), config.getScheduleTime(), TimeUnit.SECONDS);
        }
    }

    private void loaderExtPlugins() {
        try {
            List<ShenyuLoaderResult> extendPlugins = ShenyuPluginLoader.getInstance().loadExtendPlugins(shenyuConfig.getExtPlugin().getPath());
            loaderPlugins(extendPlugins);
        } catch (Exception e) {
            LOG.error("shenyu ext plugins load has error ", e);
        }
    }

    /**
     * loadUploadedJarPlugins.
     *
     * @param uploadedJarResources uploadedJarResources
     */
    public void loadUploadedJarPlugins(final List<String> uploadedJarResources) {
        try {
            List<ShenyuLoaderResult> extendPlugins = ShenyuPluginLoader.getInstance().loadUploadedJarPlugins(uploadedJarResources);
            loaderPlugins(extendPlugins);
        } catch (Exception e) {
            LOG.error("shenyu ext plugins load has error ", e);
        }
    }

    /**
     * loaderPlugins.
     *
     * @param results results
     */
    private void loaderPlugins(final List<ShenyuLoaderResult> results) {
        if (CollectionUtils.isEmpty(results)) {
            return;
        }
        List<ShenyuPlugin> shenyuExtendPlugins = results.stream().map(ShenyuLoaderResult::getShenyuPlugin).filter(Objects::nonNull).collect(Collectors.toList());
        webHandler.putExtPlugins(shenyuExtendPlugins);
        List<PluginDataHandler> handlers = results.stream().map(ShenyuLoaderResult::getPluginDataHandler).filter(Objects::nonNull).collect(Collectors.toList());
        subscriber.putExtendPluginDataHandler(handlers);
    }

}
