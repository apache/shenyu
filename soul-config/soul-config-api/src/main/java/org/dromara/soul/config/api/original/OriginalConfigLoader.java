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

package org.dromara.soul.config.api.original;

import java.util.Objects;
import java.util.function.Supplier;

import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.config.api.ConfigLoader;
import org.dromara.soul.config.api.ConfigParent;
import org.dromara.soul.config.api.bind.BindData;
import org.dromara.soul.config.api.bind.Binder;
import org.dromara.soul.config.api.bind.DataType;
import org.dromara.soul.config.api.property.ConfigPropertySource;
import org.dromara.soul.config.api.property.DefaultConfigPropertySource;
import org.dromara.soul.config.api.property.PropertyKeyParse;
import org.dromara.soul.config.api.property.PropertyKeySource;

/**
 * OriginalConfigLoader .
 *
 * @author xiaoyu
 * @author sixh
 */
public class OriginalConfigLoader implements ConfigLoader<ConfigParent> {

    OriginalConfigLoader() {
    }

    @Override
    public void load(Supplier<Context> context, LoaderHandler<ConfigParent> handler) {
        for (PropertyKeySource<?> propertyKeySource : context.get().getSource()) {
            ConfigPropertySource configPropertySource = new DefaultConfigPropertySource(propertyKeySource, PropertyKeyParse.INSTANCE);
            ConfigEnv.getInstance().stream().map(e -> {
                Binder binder = Binder.of(configPropertySource);
                return binder.bind(e.prefix(), BindData.of(DataType.of(e.getClass()), () -> e));
            }).filter(Objects::nonNull).peek(ConfigParent::flagLoad).forEach(e -> handler.finish(context, e));
        }
    }
}
