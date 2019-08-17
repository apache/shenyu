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


import org.dromara.soul.config.api.bind.BindData;
import org.dromara.soul.config.api.bind.Binder;
import org.dromara.soul.config.api.bind.DataType;
import org.dromara.soul.config.api.original.OriginalConfigLoader;
import org.dromara.soul.config.api.property.ConfigPropertySource;
import org.dromara.soul.config.api.property.DefaultConfigPropertySource;
import org.dromara.soul.config.api.property.PropertyKeyParse;
import org.dromara.soul.config.api.property.PropertyKeySource;

import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * ConfigLoader .
 * Different configuration adaptation processes are implemented by configuring interfaces.
 * 1. Custom implementation.
 *
 * @author sixh
 */
public abstract class ConfigLoader<T extends ConfigParent> {


    /**
     * Load related configuration information.
     *
     * @return 配置信息.
     */
    public abstract void load(Supplier<Context> context, LoaderHandler<T> handler);

    protected void againLoad(Supplier<Context> context, LoaderHandler<T> handler, Class<T> tClass) {
        T config = ConfigEnv.getInstance().getConfig(tClass);
        for (PropertyKeySource<?> propertyKeySource : context.get().getSource()) {
            ConfigPropertySource configPropertySource = new DefaultConfigPropertySource(propertyKeySource, PropertyKeyParse.INSTANCE);
            Binder binder = Binder.of(configPropertySource);
            T bind = binder.bind(config.prefix(), BindData.of(DataType.of(tClass), () -> config));
            handler.finish(context, bind);
        }
    }

    public static class Context {

        private ConfigLoader<ConfigParent> original;

        private List<PropertyKeySource<?>> propertyKeySources;

        public Context() {
        }

        public Context(List<PropertyKeySource<?>> propertyKeySources) {
            this(null, propertyKeySources);
        }

        public Context(ConfigLoader<ConfigParent> original, List<PropertyKeySource<?>> propertyKeySources) {
            this.original = original;
            this.propertyKeySources = propertyKeySources;
        }

        public Context with(List<PropertyKeySource<?>> sources, ConfigLoader<ConfigParent> original) {
            return new Context(original, sources);
        }

        public Context withSources(List<PropertyKeySource<?>> sources) {
            return with(sources, this.original);
        }

        public ConfigLoader<ConfigParent> getOriginal() {
            return original;
        }

        public List<PropertyKeySource<?>> getSource() {
            return propertyKeySources;
        }
    }

    @FunctionalInterface
    public interface LoaderHandler<T extends ConfigParent> {
        /**
         * 加载完成后要做的事情啊.
         *
         * @param config config.
         */
        void finish(Supplier<Context> context, T config);
    }
}
