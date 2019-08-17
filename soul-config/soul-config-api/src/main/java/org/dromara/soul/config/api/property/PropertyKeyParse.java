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

package org.dromara.soul.config.api.property;

import org.dromara.soul.common.utils.ObjectUtils;

/**
 * PropertyKeyParse .
 * 将{@linkplain PropertyName} to {@linkplain PropertyKey}
 * 2019-08-15
 *
 * @author sixh
 */
public enum PropertyKeyParse {
    /**
     * instance.
     */
    INSTANCE;

    private LastKey<PropertyName> lastKeyObj;

    private LastKey<String> lastKeyStr;

    public static PropertyKeyParse getInstance() {
        return INSTANCE;
    }

    public PropertyKey[] parse(PropertyName propertyName) {
        LastKey<PropertyName> last = this.lastKeyObj;
        if (last != null && last.isFrom(propertyName)) {
            return last.getKeys();
        }
        String name = propertyName.getName();
        PropertyKey[] propertyKeys = {new PropertyKey(name, propertyName)};
        this.lastKeyObj = new LastKey<>(propertyName, propertyKeys);
        return propertyKeys;
    }

    public PropertyKey[] parse(String propertyName) {
        // Use a local copy in case another thread changes things
        LastKey<String> last = this.lastKeyStr;
        if (last != null && last.isFrom(propertyName)) {
            return last.getKeys();
        }
        PropertyKey[] mapping = tryMap(propertyName);
        this.lastKeyStr = new LastKey<>(propertyName, mapping);
        return mapping;
    }

    private PropertyKey[] tryMap(String propertyName) {
        PropertyName name = PropertyName.of(propertyName);
        if (!name.isEmpty()) {
            return new PropertyKey[]{new PropertyKey(propertyName, name)};
        }

        return new PropertyKey[0];
    }

    private static class LastKey<T> {

        private final T from;

        private final PropertyKey[] keys;

        LastKey(T from, PropertyKey[] keys) {
            this.from = from;
            this.keys = keys;
        }

        boolean isFrom(T from) {
            return ObjectUtils.nullSafeEquals(from, this.from);
        }

        PropertyKey[] getKeys() {
            return this.keys;
        }
    }
}
