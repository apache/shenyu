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

package org.dromara.soul.config.api.bind;

import org.dromara.soul.config.api.property.ConfigPropertySource;
import org.dromara.soul.config.api.property.PropertyName;
import org.dromara.soul.common.utils.CollectionUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

/**
 * MapBinder .
 *
 * @author sixh
 */
public final class MapBinder extends AggregateBinder<Map<Object, Object>> {

    MapBinder(Binder.Env env) {
        super(env);
    }

    @Override
    Object bind(PropertyName propertyName, BindData<?> target, Binder.Env env, AggregateElementBinder elementBinder) {
        Map<Object, Object> map = new HashMap<>(16);
        boolean hasDescendants = env.getSource().containsDescendantOf(propertyName);
        if (!hasDescendants) {
            new EntryBinder(propertyName, target, env, elementBinder).bindEntries(env.getSource(), map);
        }
        return (map.isEmpty() ? null : map);
    }

    @Override
    Object merge(Supplier<?> targetValue, Map<Object, Object> object) {
        Map<Object, Object> existingMap = getExistingIfPossible(targetValue);
        if (existingMap == null) {
            return object;
        }
        try {
            existingMap.putAll(object);
            return copyIfPossible(existingMap);
        } catch (UnsupportedOperationException ex) {
            Map<Object, Object> result = createNewMap(object.getClass(), existingMap);
            result.putAll(object);
            return result;
        }
    }

    @SuppressWarnings("unchecked")
    private Map<Object, Object> getExistingIfPossible(Supplier<?> existing) {
        try {
            return (Map<Object, Object>) existing.get();
        } catch (Exception ex) {
            return null;
        }
    }

    private Map<Object, Object> copyIfPossible(Map<Object, Object> map) {
        try {
            return createNewMap(map.getClass(), map);
        } catch (Exception ex) {
            return map;
        }
    }

    private Map<Object, Object> createNewMap(Class<?> mapClass, Map<Object, Object> map) {
        Map<Object, Object> result = CollectionUtils.createFactory().createMap(mapClass, map.size());
        result.putAll(map);
        return result;
    }

    @Override
    public boolean isAllowRecursiveBinding(Binder.Env source) {
        return false;
    }

    private static class EntryBinder {

        private final PropertyName root;

        private final AggregateElementBinder elementBinder;

        private final DataType mapType;

        private final DataType keyType;

        private final DataType valueType;

        private final Binder.Env env;

        EntryBinder(PropertyName root,
                    BindData<?> target,
                    Binder.Env env,
                    AggregateElementBinder elementBinder) {
            this.root = root;
            this.elementBinder = elementBinder;
            this.mapType = target.getType();
            DataType[] generics = target.getType().getGenerics();
            this.keyType = generics.length > 1 ? generics[0] : DataType.of(Object.class);
            this.valueType = generics.length > 1 ? generics[1] : DataType.of(Object.class);
            this.env = env;
        }

        void bindEntries(ConfigPropertySource source,
                         Map<Object, Object> map) {
            source.stream().forEach(name -> {
                boolean ancestorOf = root.isAncestorOf(name);
                if (ancestorOf) {
                    BindData<?> valueBindData = getValueBindData(name);
                    PropertyName entryName = getEntryName(source, name);
                    Object key = getKeyName(entryName);
                    map.computeIfAbsent(key,
                            (k) -> this.elementBinder.bind(entryName, valueBindData, this.env));
                }
            });
        }

        private BindData<?> getValueBindData(PropertyName name) {
            if (!this.root.isParentOf(name) && isValueTreatedAsNestedMap()) {
                return BindData.of(this.mapType);
            }
            return BindData.of(this.valueType);
        }

        private PropertyName getEntryName(ConfigPropertySource source, PropertyName name) {
            if (valueType.isCollection() || valueType.isArray()) {
                return chopNameAtNumericIndex(name);
            }
            if (!this.root.isParentOf(name) && (isValueTreatedAsNestedMap() || !isScalarValue(source, name))) {
                return name.chop(this.root.getElementSize() + 1);
            }
            return name;
        }

        private PropertyName chopNameAtNumericIndex(
                PropertyName name) {
            int start = this.root.getElementSize() + 1;
            int size = name.getElementSize();
            for (int i = start; i < size; i++) {
                if (name.isNumericIndex(i)) {
                    return name.chop(i);
                }
            }
            return name;
        }

        private boolean isValueTreatedAsNestedMap() {
            return false;
        }

        private boolean isScalarValue(ConfigPropertySource source, PropertyName name) {
            return true;
        }

        private String getKeyName(PropertyName name) {
            StringBuilder result = new StringBuilder();
            for (int i = this.root.getElementSize(); i < name
                    .getElementSize(); i++) {
                result.append(result.length() != 0 ? "." : "");
                result.append(name.getElement(i));
            }
            return result.toString();
        }

    }

}
