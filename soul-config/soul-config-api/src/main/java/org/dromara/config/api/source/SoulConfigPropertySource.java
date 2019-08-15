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

import org.dromara.config.api.PropertySource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

/**
 * SoulConfigPropertySource .
 *
 * @author sixh
 */
public class SoulConfigPropertySource implements ConfigPropertySource {

    private PropertySource<?> propertySource;

    private PropertyKeyMapper mapper;

    public SoulConfigPropertySource(PropertySource<?> propertySource,
                                    PropertyKeyMapper mapper) {
        this.propertySource = propertySource;
        this.mapper = mapper;
    }

    @Override
    public ConfigProperty findProperty(PropertyName name) {
        PropertyKey[] map = getMapper().map(name);
        return find(map, name);
    }

    @Override
    public Stream<PropertyName> stream() {
        return getPropertyNames().stream();
    }

    private final ConfigProperty find(PropertyKey[] mappings,
                                      PropertyName name) {
        for (PropertyKey candidate : mappings) {
            if (candidate.isApplicable(name)) {
                ConfigProperty result = find(candidate);
                if (result != null) {
                    return result;
                }
            }
        }
        return null;
    }

    private List<PropertyName> getPropertyNames() {
        List<PropertyKey> propertyKeys = getPropertyKeys();
        List<PropertyName> names = new ArrayList<>(propertyKeys.size());
        propertyKeys.forEach(propertyKey -> names.add(propertyKey.getPropertyNameObj()));
        return names;
    }

    private List<PropertyKey> getPropertyKeys() {
        List<String> names = getPropertySource().getPropertyKeys();
        List<PropertyKey> mappings = new ArrayList<>(names.size() * 2);
        for (String name : names) {
            mappings.addAll(Arrays.asList(getMapper().map(name)));
        }
        return mappings;
    }

    @Override
    public boolean containsDescendantOf(PropertyName name) {
        List<PropertyName> propertyNames = getPropertyNames();
        for (PropertyName thisName : propertyNames) {
            if (thisName.isAncestorOf(name)) {
                return true;
            }
        }
        return false;
    }

    private ConfigProperty find(PropertyKey mapping) {
        String propertySourceName = mapping.getPropertyName();
        Object value = getPropertySource().getProperty(propertySourceName);
        if (value == null) {
            return null;
        }
        PropertyName configurationPropertyName = mapping
                .getPropertyNameObj();
        return ConfigProperty.of(configurationPropertyName, value);
    }

    public PropertyKeyMapper getMapper() {
        return mapper;
    }

    public PropertySource<?> getPropertySource() {
        return propertySource;
    }
}
