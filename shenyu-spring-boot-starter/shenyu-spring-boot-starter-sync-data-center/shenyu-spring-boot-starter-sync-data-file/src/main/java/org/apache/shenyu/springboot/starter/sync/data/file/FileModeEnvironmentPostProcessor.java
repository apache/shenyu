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

package org.apache.shenyu.springboot.starter.sync.data.file;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.context.config.ConfigDataEnvironmentPostProcessor;
import org.springframework.boot.env.EnvironmentPostProcessor;
import org.springframework.core.Ordered;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.MapPropertySource;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Establishes exclusive file ownership before any client or gateway bean is created.
 */
public final class FileModeEnvironmentPostProcessor implements EnvironmentPostProcessor, Ordered {

    static final String ENABLED = "shenyu.sync.file.enabled";

    @Override
    public void postProcessEnvironment(final ConfigurableEnvironment environment, final SpringApplication application) {
        if (!environment.getProperty(ENABLED, Boolean.class, false)) {
            return;
        }
        Map<String, Object> overrides = new LinkedHashMap<>();
        for (String channel : List.of("websocket.urls", "http.url", "zookeeper.url", "nacos.url", "etcd.url", "apollo.meta")) {
            String key = "shenyu.sync." + channel;
            String value = environment.getProperty(key, "false");
            // The shipped bootstrap Admin URL is an inherited default, not an additional source.
            boolean bootstrapDefault = "websocket.urls".equals(channel) && "ws://localhost:9095/websocket".equals(value);
            if (!value.isBlank() && !"false".equalsIgnoreCase(value) && !bootstrapDefault) {
                throw new IllegalStateException("File configuration conflicts with " + key + "; remove the external sync source");
            }
            overrides.put(key, "false");
        }
        for (String key : List.of("shenyu.register.enabled", "shenyu.local.enabled", "shenyu.alert.enabled")) {
            if (environment.getProperty(key, Boolean.class, false)) {
                throw new IllegalStateException("File configuration conflicts with " + key + "; disable the competing writer/client");
            }
            overrides.put(key, "false");
        }
        // These features are enabled in the stock bootstrap. File mode replaces them explicitly.
        overrides.put("shenyu.heartbeat.enabled", "false");
        overrides.put("shenyu.extPlugin.enabled", "false");
        overrides.put("shenyu.sync.websocket.initial-sync-readiness", "false");
        environment.getPropertySources().addFirst(new MapPropertySource("shenyuFileMode", overrides));
    }

    @Override
    public int getOrder() {
        return ConfigDataEnvironmentPostProcessor.ORDER + 1;
    }
}
