/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.dromara.soul.config.api;

import org.dromara.soul.common.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

/**
 * ConfigEnv .
 * 配置文件的处理.
 *
 * @author sixh
 */
public class ConfigEnv {

    private static final ConfigEnv INST = new ConfigEnv();

    private static final Logger logger = LoggerFactory.getLogger(ConfigEnv.class);

    private final Map<Class, ConfigParent> configBeans = new ConcurrentHashMap<>();

    /**
     * 保存一些自定义的配置信息.
     */
    private final Set<String> configClassName = new HashSet<>();

    private ConfigEnv() {
        if (INST != null) {
            throw new ConfigException("repeated configEnv object.");
        }
    }

    public static ConfigEnv getInstance() {
        return INST;
    }

    /**
     * 增加一个需要处理的class Path.
     *
     * @param classPath class path.
     */
    public void addConfigClassPath(String classPath) {
        if (classPath.startsWith("java.")) {
            logger.warn("config class path 忽略的 {}", classPath);
            return;
        }
        addConfigClassPath(classPath, true);
    }

    private void addConfigClassPath(String classPath, boolean instantiation) {
        if (!instantiation) {
            configClassName.add(classPath);
            return;
        }
        try {
            Class<?> clazz = Class.forName(classPath);
            if (clazz.getSuperclass().isAssignableFrom(ConfigParent.class)) {
                ConfigParent configParent = (ConfigParent) clazz.newInstance();
                putBean(configParent);
                configClassName.add(classPath);
            }
        } catch (ClassNotFoundException | IllegalAccessException | InstantiationException e) {
            throw new ConfigException(e);
        }
    }

    @SuppressWarnings("unchecked")
    public <T extends ConfigParent> T getConfig(Class<T> clazz) {
        return (T) configBeans.get(clazz);
    }

    /**
     * 注删一个需要解释配置信息的对象 .
     *
     * @param parent parent.
     */
    public void putBean(ConfigParent parent) {
        if (parent != null && StringUtils.isNotBlank(parent.prefix())) {
            if (configBeans.containsKey(parent.getClass())) {
                return;
            }
            configBeans.put(parent.getClass(), parent);
            addConfigClassPath(parent.getClass().getName(), false);
        }
    }

    /**
     * 获取所有已经加载的配置信息.
     *
     * @return stream.
     */
    public Stream<ConfigParent> stream() {
        return configBeans.values().stream().filter(e -> !e.isLoad());
    }
}
