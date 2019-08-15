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

package org.dromara.config.core.bind;

import org.dromara.config.core.property.ConfigProperty;
import org.dromara.config.core.property.ConfigPropertySource;
import org.dromara.config.core.property.PropertyName;

/**
 * SoluBinder .
 * <p>
 * 2019-08-15 22:11
 *
 * @author chenbin sixh
 */
public class Binder {

    private ConfigPropertySource source;

    private Binder(ConfigPropertySource source) {
        this.source = source;
    }

    /**
     * 构造一个属性binder器.
     *
     * @param source source.
     * @return
     */
    public static Binder of(ConfigPropertySource source) {
        return new Binder(source);
    }

    public <T> T bind(String propertyName, BindData<T> target) {
        return bind(PropertyName.of(propertyName), target);
    }

    public <T> T bind(PropertyName propertyName, BindData<T> target) {
        Env env = new Env();
        return bindObject(propertyName, target, env);
    }

    private <T> T bindObject(PropertyName propertyName, BindData<T> target, Env env) {
        ConfigProperty property = findProperty(propertyName, env);
        return null;
    }

    private static ConfigProperty findProperty(PropertyName propertyName, Env env) {
        if (propertyName.isEmpty()) {
            return null;
        }
        ConfigProperty property = env.getSource().findProperty(propertyName);
        if(property == null && env.getSource())
        return null;
    }

    class Env {
        /**
         * sources.
         */
        private ConfigPropertySource source;

        private ConfigProperty property;

        public ConfigPropertySource getSource() {
            if (source == null) {
                return Binder.this.source;
            }
            return source;
        }
    }
}
