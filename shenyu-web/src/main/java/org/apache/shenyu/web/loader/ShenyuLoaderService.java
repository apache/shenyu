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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.isolation.ExtendDataBase;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.cache.ExtendDataHandler;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * The type Shenyu loader service.
 */
public class ShenyuLoaderService {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuLoaderService.class);

    private final ShenyuWebHandler webHandler;

    private final ShenyuConfig shenyuConfig;

    private final List<ExtendDataHandler<?>> extendDataHandlers;

    /**
     * Instantiates a new Shenyu loader service.
     *
     * @param webHandler         the web handler
     * @param shenyuConfig       the shenyu config
     * @param extendDataHandlers addDataHandlers
     */
    public ShenyuLoaderService(final ShenyuWebHandler webHandler, final ShenyuConfig shenyuConfig, final List<ExtendDataHandler<?>> extendDataHandlers) {
        this.webHandler = webHandler;
        this.extendDataHandlers = extendDataHandlers;
        this.shenyuConfig = shenyuConfig;
        ExtPlugin config = shenyuConfig.getExtPlugin();
        if (config.getEnabled()) {
            ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(config.getThreads(), ShenyuThreadFactory.create("plugin-ext-loader", true));
            executor.scheduleAtFixedRate(() -> loadExtOrUploadPlugins(null), config.getScheduleDelay(), config.getScheduleTime(), TimeUnit.SECONDS);
        }
    }

    /**
     * loadPlugin from ext-lib or admin upload jar.
     *
     * @param uploadedJarResource uploadedJarResource is null load ext-lib,not null load admin upload jar
     */
    public void loadExtOrUploadPlugins(final PluginData uploadedJarResource) {
        try {
            List<ShenyuLoaderResult> plugins = new ArrayList<>();
            ShenyuPluginClassloaderHolder singleton = ShenyuPluginClassloaderHolder.getSingleton();
            if (Objects.isNull(uploadedJarResource)) {
                List<PluginJarParser.PluginJar> uploadPluginJars = ShenyuExtPathPluginJarLoader.loadExtendPlugins(shenyuConfig.getExtPlugin().getPath());
                for (PluginJarParser.PluginJar extPath : uploadPluginJars) {
                    LOG.info("shenyu extPlugin find new {} to load", extPath.getAbsolutePath());
                    ShenyuPluginClassLoader extPathClassLoader = singleton.createPluginClassLoader(extPath);
                    plugins.addAll(extPathClassLoader.loadUploadedJarPlugins(this.getClass().getClassLoader()));
                }
            } else {
                PluginJarParser.PluginJar pluginJar = PluginJarParser.parseJar(Base64.getDecoder().decode(uploadedJarResource.getPluginJar()));
                LOG.info("shenyu upload plugin jar find new {} to load", pluginJar.getJarKey());
                ShenyuPluginClassLoader uploadPluginClassLoader = singleton.createPluginClassLoader(pluginJar);
                plugins.addAll(uploadPluginClassLoader.loadUploadedJarPlugins(this.getClass().getClassLoader()));
            }
            loaderPlugins(plugins);
        } catch (Exception e) {
            LOG.error("shenyu plugins load has error ", e);
        }
    }

    /**
     * loadJarPlugins.
     *
     * @param parseJarInputStream parseJarInputStream
     * @param classLoader classLoader
     * @return a list of ShenyuLoaderResult
     */
    public List<ShenyuLoaderResult> loadJarPlugins(final InputStream parseJarInputStream, final ClassLoader classLoader) {
        try {
            ShenyuPluginClassloaderHolder singleton = ShenyuPluginClassloaderHolder.getSingleton();
            PluginJarParser.PluginJar pluginJar = PluginJarParser.parseJar(parseJarInputStream);
            ShenyuPluginClassLoader shenyuPluginClassLoader = singleton.createPluginClassLoader(pluginJar);
            List<ShenyuLoaderResult> uploadPlugins = shenyuPluginClassLoader.loadUploadedJarPlugins(classLoader);
            loaderPlugins(uploadPlugins);
            return uploadPlugins;
        } catch (Exception e) {
            LOG.error("Shenyu upload plugins load has error ", e);
            return Collections.emptyList();
        }
    }

    /**
     * removeJarPlugins.
     *
     * @param parseJarInputStream parseJarInputStream
     */
    public void removeJarPlugins(final InputStream parseJarInputStream) {
        ShenyuPluginClassloaderHolder singleton = ShenyuPluginClassloaderHolder.getSingleton();
        PluginJarParser.PluginJar pluginJar = PluginJarParser.parseJar(parseJarInputStream);
        String jarKey = Optional.ofNullable(pluginJar.getAbsolutePath()).orElse(pluginJar.getJarKey());
        singleton.removePluginClassLoader(jarKey);
    }

    /**
     * loaderPlugins.
     *
     * @param results results
     */
    public void loaderPlugins(final List<ShenyuLoaderResult> results) {
        if (CollectionUtils.isEmpty(results)) {
            return;
        }
        List<ShenyuPlugin> shenyuExtendPlugins = results.stream().map(ShenyuLoaderResult::getShenyuPlugin).filter(Objects::nonNull).collect(Collectors.toList());
        webHandler.putExtPlugins(shenyuExtendPlugins);
        List<ExtendDataBase> handlers = results.stream().map(ShenyuLoaderResult::getExtendDataBase)
                .filter(Objects::nonNull).collect(Collectors.toList());
        extendDataHandlers.forEach(addDataHandlers1 -> addDataHandlers1.putExtendDataHandler(handlers));
    }

}
