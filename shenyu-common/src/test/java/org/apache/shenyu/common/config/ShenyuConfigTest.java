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

package org.apache.shenyu.common.config;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.springframework.util.Assert;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

/**
 * Test cases for ShenyuConfig.
 */
public class ShenyuConfigTest {
    private final ShenyuConfig config = new ShenyuConfig();

    /**
     * test of shenyuConfig.
     *
     * @return shenyuConfig
     */
    public ShenyuConfig getConfig() {
        return this.config;
    }

    @Test
    public void testShenyuConfig() {
        ShenyuConfig.CrossFilterConfig cross = config.getCross();
        ShenyuConfig.SwitchConfig switchConfig = config.getSwitchConfig();
        ShenyuConfig.ExcludePath exclude = config.getExclude();
        ShenyuConfig.FallbackPath fallback = config.getFallback();
        ShenyuConfig.FileConfig file = config.getFile();
        ShenyuConfig.InstanceConfig instance = config.getInstance();
        ShenyuConfig.ExtPlugin extPlugin = config.getExtPlugin();
        ShenyuConfig.Local local = config.getLocal();
        ShenyuConfig.RibbonConfig ribbon = config.getRibbon();
        ShenyuConfig.MetricsConfig metrics = config.getMetrics();
        ShenyuConfig.Scheduler scheduler = config.getScheduler();
        ShenyuConfig.SharedPool sharedPool = config.getSharedPool();
        ShenyuConfig.WebsocketConfig websocket = config.getWebsocket();
        ShenyuConfig.UpstreamCheck upstreamCheck = config.getUpstreamCheck();

        notEmptyElements(cross, switchConfig, exclude, fallback, file, instance,
                extPlugin, local, ribbon, metrics, scheduler, sharedPool, websocket, upstreamCheck);
    }

    private void notEmptyElements(final Object... objects) {
        Assert.isTrue(ArrayUtils.isNotEmpty(objects), "array must not be empty");

        Arrays.stream(objects).forEach(val -> {
            Assert.notNull(val, "val must not be null");
            if (val instanceof String) {
                Assert.isTrue(StringUtils.isNotEmpty((String) val), "val must not be empty");
            }
        });
    }

    @Test
    public void testUpstreamCheck() {
        ShenyuConfig.UpstreamCheck upstreamCheck = config.getUpstreamCheck();
        upstreamCheck.setEnabled(false);
        upstreamCheck.setHealthyThreshold(4);
        upstreamCheck.setTimeout(10);
        upstreamCheck.setInterval(5);
        upstreamCheck.setUnhealthyThreshold(5);
        upstreamCheck.setPrintEnabled(false);
        upstreamCheck.setPrintInterval(5);

        notEmptyElements(upstreamCheck.getEnabled(), upstreamCheck.getHealthyThreshold(), upstreamCheck.getTimeout(),
                upstreamCheck.getInterval(), upstreamCheck.getUnhealthyThreshold(), upstreamCheck.getPrintInterval(), upstreamCheck.getPrintEnabled());
    }

    @Test
    public void testWebsocketConfig() {
        ShenyuConfig.WebsocketConfig websocket = config.getWebsocket();
        websocket.setMaxFramePayloadSize(5);
        Assert.isTrue(websocket.getMaxFramePayloadSize() == 5, "result not excepted");
    }

    @Test
    public void testSharedPool() {
        ShenyuConfig.SharedPool sharedPool = config.getSharedPool();
        sharedPool.setCorePoolSize(3);
        sharedPool.setEnable(true);
        sharedPool.setMaximumPoolSize(5);
        sharedPool.setPrefix("test-");
        sharedPool.setKeepAliveTime(1000L);
        sharedPool.setMaxWorkQueueMemory(1024L);

        notEmptyElements(sharedPool.getCorePoolSize(), sharedPool.getEnable(), sharedPool.getMaximumPoolSize(),
                sharedPool.getPrefix(), sharedPool.getKeepAliveTime(), sharedPool.getMaxWorkQueueMemory());
    }

    @Test
    public void testScheduler() {
        ShenyuConfig.Scheduler scheduler = config.getScheduler();
        scheduler.setEnabled(true);
        scheduler.setThreads(5);
        scheduler.setType("test");

        Boolean enabled = scheduler.getEnabled();
        Integer threads = scheduler.getThreads();
        String type = scheduler.getType();

        notEmptyElements(enabled, type, threads);
    }

    @Test
    public void testMetricsConfig() {
        ShenyuConfig.MetricsConfig metrics = config.getMetrics();
        metrics.setPort(123);
        metrics.setEnabled(true);
        metrics.setName("test");
        metrics.setHost("test");
        metrics.setJmxConfig("jmxConfig");
        metrics.setProps(new Properties());

        boolean enabled = metrics.getEnabled();
        Assert.isTrue(enabled, "result not excepted");

        String jmxConfig = metrics.getJmxConfig();
        Properties props = metrics.getProps();
        String host = metrics.getHost();
        String name = metrics.getName();
        Integer port = metrics.getPort();

        notEmptyElements(jmxConfig, props, host, name, port);
    }

    @Test
    public void testRibbonConfig() {
        ShenyuConfig.Local local = config.getLocal();
        local.setEnabled(true);
        local.setSha512Key("test");
        Boolean enabled = local.getEnabled();
        String sha512Key = local.getSha512Key();

        notEmptyElements(enabled, sha512Key);
    }

    @Test
    public void testLocal() {
        ShenyuConfig.Local local = config.getLocal();
        local.setEnabled(true);
        local.setSha512Key("test");
        Boolean enabled = local.getEnabled();
        String sha512Key = local.getSha512Key();

        notEmptyElements(enabled, sha512Key);
    }

    @Test
    public void testExtPlugin() {
        ShenyuConfig.ExtPlugin extPlugin = config.getExtPlugin();
        extPlugin.setThreads(5);
        extPlugin.setPath("test");
        extPlugin.setEnabled(true);
        extPlugin.setScheduleDelay(5);

        String path = extPlugin.getPath();
        Integer threads = extPlugin.getThreads();
        Boolean enabled = extPlugin.getEnabled();
        Integer scheduleDelay = extPlugin.getScheduleDelay();
        Integer scheduleTime = extPlugin.getScheduleTime();

        notEmptyElements(enabled, path, scheduleTime, scheduleDelay, threads);
    }

    @Test
    public void testInstanceConfig() {
        ShenyuConfig.InstanceConfig instance = config.getInstance();
        instance.setEnabled(true);
        instance.setServerLists("test");
        instance.setRegisterType("test");
        instance.setProps(new Properties());

        Boolean enabled = instance.getEnabled();
        Properties props = instance.getProps();
        String registerType = instance.getRegisterType();
        String serverLists = instance.getServerLists();

        notEmptyElements(props, registerType, serverLists, enabled);
    }

    @Test
    public void testFileConfig() {
        ShenyuConfig.FileConfig fileConfig = config.getFile();
        fileConfig.setMaxSize(10);
        fileConfig.setEnabled(true);

        Boolean enabled = fileConfig.getEnabled();
        Integer maxSize = fileConfig.getMaxSize();

        notEmptyElements(maxSize, enabled);
    }

    @Test
    public void testFallbackPath() {
        ShenyuConfig.FallbackPath fallback = config.getFallback();
        fallback.setEnabled(true);
        fallback.setPaths(Collections.emptyList());

        List<String> paths = fallback.getPaths();
        Boolean enabled = fallback.getEnabled();

        notEmptyElements(paths, enabled);
    }

    @Test
    public void testExcludePath() {
        ShenyuConfig.ExcludePath exclude = config.getExclude();
        exclude.setEnabled(true);
        exclude.setPaths(Collections.emptyList());

        List<String> paths = exclude.getPaths();
        Boolean enabled = exclude.getEnabled();

        notEmptyElements(paths, enabled);
    }

    @Test
    public void testSwitchConfig() {
        ShenyuConfig.SwitchConfig switchConfig = config.getSwitchConfig();
        switchConfig.setLocal(true);
        Boolean local = switchConfig.getLocal();

        notEmptyElements(local);
    }

    @Test
    public void testCrossFilterConfig() {
        ShenyuConfig.CrossFilterConfig cross = config.getCross();
        cross.setAllowCredentials(false);
        cross.setEnabled(false);
        cross.setAllowedExpose("test");
        cross.setAllowedMethods("test");
        cross.setAllowedHeaders("test");
        cross.setMaxAge("test");

        String allowedExpose = cross.getAllowedExpose();
        String allowedHeaders = cross.getAllowedHeaders();
        ShenyuConfig.CrossFilterConfig.AllowedOriginConfig allowedOrigin = cross.getAllowedOrigin();
        Boolean enabled = cross.getEnabled();
        String maxAge = cross.getMaxAge();
        String allowedMethods = cross.getAllowedMethods();

        notEmptyElements(allowedExpose, allowedHeaders, allowedOrigin, enabled, maxAge, allowedMethods);
    }
}
