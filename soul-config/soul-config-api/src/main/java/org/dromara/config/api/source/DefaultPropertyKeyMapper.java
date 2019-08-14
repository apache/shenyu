/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.config.api.source;

import org.dromara.soul.common.utils.ObjectUtils;

/**
 * DefaultPropertyKeyMapper .
 *
 * @author sixh
 */
public class DefaultPropertyKeyMapper implements PropertyKeyMapper {

    public static final DefaultPropertyKeyMapper INSTANCE = new DefaultPropertyKeyMapper();

    private LastMapping<PropertyName> lastMappedConfigurationPropertyName;

    private LastMapping<String> lastMappedPropertyName;

    private DefaultPropertyKeyMapper() {

    }

    @Override
    public PropertyKey[] map(PropertyName propertyName) {
        // Use a local copy in case another thread changes things
        LastMapping<PropertyName> last = this.lastMappedConfigurationPropertyName;
        if (last != null && last.isFrom(propertyName)) {
            return last.getMapping();
        }
        String convertedName = propertyName.getName();
        PropertyKey[] mapping = {
                new PropertyKey(convertedName, propertyName)};
        this.lastMappedConfigurationPropertyName = new LastMapping<>(
                propertyName, mapping);
        return mapping;
    }

    @Override
    public PropertyKey[] map(String name) {
        // Use a local copy in case another thread changes things
        LastMapping<String> last = this.lastMappedPropertyName;
        if (last != null && last.isFrom(name)) {
            return last.getMapping();
        }
        PropertyKey[] mapping = tryMap(name);
        this.lastMappedPropertyName = new LastMapping<>(name, mapping);
        return mapping;
    }

    private PropertyKey[] tryMap(String propertySourceName) {
        try {
            PropertyName convertedName = PropertyName
                    .adapt(propertySourceName, '.');
            if (!convertedName.isEmpty()) {
                return new PropertyKey[]{
                        new PropertyKey(propertySourceName, convertedName)};
            }
        } catch (Exception ex) {
        }
        return new PropertyKey[0];
    }


    private static class LastMapping<T> {

        private final T from;

        private final PropertyKey[] mapping;

        LastMapping(T from, PropertyKey[] mapping) {
            this.from = from;
            this.mapping = mapping;
        }

        public boolean isFrom(T from) {
            return ObjectUtils.nullSafeEquals(from, this.from);
        }

        public PropertyKey[] getMapping() {
            return this.mapping;
        }

    }
}
