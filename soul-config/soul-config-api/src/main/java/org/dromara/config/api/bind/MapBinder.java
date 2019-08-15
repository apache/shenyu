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

package org.dromara.config.api.bind;

import org.apache.commons.lang3.ClassUtils;
import org.dromara.config.api.source.ConfigProperty;
import org.dromara.config.api.source.ConfigPropertySource;
import org.dromara.config.api.source.PropertyName;
import org.dromara.soul.common.utils.CollectionUtils;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

/**
 * MapBinder .
 * <p>
 * <p>
 * 2019-08-13 21:10
 *
 * @author chenbin sixh
 */
public class MapBinder extends AggregateBinder<Map> {


    @Override
    Object bindAggregate(PropertyName name, BindData<?> target, AggregateElementBinder elementBinder) {
        Map<Object, Object> map = new HashMap<>();
        boolean hasDescendants = getEnv().stream().anyMatch((source) -> source
                .containsDescendantOf(name));
        if (!hasDescendants) {
            for (ConfigPropertySource source : getEnv().getSources()) {
                new EntryBinder(name, target, elementBinder).bindEntries(source, map);
            }
        }
        return (map.isEmpty() ? null : map);
    }


    @Override
    Map assemble(Supplier<?> inst, Map additional) {
        Map<Object, Object> existingMap = getExistingIfPossible(inst);
        if (existingMap == null) {
            return additional;
        }
        try {
            existingMap.putAll(additional);
            return copyIfPossible(existingMap);
        } catch (UnsupportedOperationException ex) {
            Map<Object, Object> result = createNewMap(additional.getClass(), existingMap);
            result.putAll(additional);
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

    public MapBinder(Binder.Env env) {
        super(env);
    }

    @Override
    protected boolean isAllowRecursiveBinding(ConfigPropertySource source) {
        return false;
    }

    private class EntryBinder {

        private final PropertyName root;

        private final AggregateElementBinder elementBinder;

        private final Type mapType;

        private final Type keyType;

        private final Type valueType;

        EntryBinder(PropertyName root, BindData<?> target,
                    AggregateElementBinder elementBinder) {
            this.root = root;
            this.elementBinder = elementBinder;
            this.mapType = target.getType();
            Type[] generics = target.getGenerics();

            this.keyType = generics.length > 1 ? generics[0] : Object.class;
            this.valueType = generics.length > 1 ? generics[1] : Object.class;
        }

        public void bindEntries(ConfigPropertySource source,
                                Map<Object, Object> map) {
            source.stream().forEach(name -> {
                boolean ancestorOf = root.isAncestorOf(name);
                if (ancestorOf) {
                    BindData<?> valueBindData = getValueBindData(name);
                    PropertyName entryName = getEntryName(source, name);
                    Object key = getKeyName(entryName);
                    map.computeIfAbsent(key,
                            (k) -> this.elementBinder.bind(entryName, valueBindData));
                }
            });
        }

        private BindData<?> getValueBindData(PropertyName name) {
            if (!this.root.isParentOf(name) && isValueTreatedAsNestedMap()) {
                return BindData.of(this.mapType);
            }
            return BindData.of(this.valueType);
        }

        private PropertyName getEntryName(ConfigPropertySource source,
                                          PropertyName name) {
            Class<?> resolved = (Class<?>) valueType;
            if (Collection.class.isAssignableFrom(resolved) || isArray(valueType)) {
                return chopNameAtNumericIndex(name);
            }
            if (!this.root.isParentOf(name)
                    && (isValueTreatedAsNestedMap() || !isScalarValue(source, name))) {
                return name.chop(this.root.getElementsLength() + 1);
            }
            return name;
        }

        private boolean isArray(Type type) {
            return (type instanceof Class && ((Class<?>) type).isArray());
        }

        private PropertyName chopNameAtNumericIndex(
                PropertyName name) {
            int start = this.root.getElementsLength() + 1;
            int size = name.getElementsLength();
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

        private boolean isScalarValue(ConfigPropertySource source,
                                      PropertyName name) {
            return true;
            /*Class<?> resolved = this.valueType.resolve(Object.class);
            String packageName = ClassUtils.getPackageName(resolved);
            if (!packageName.startsWith("java.lang") && !resolved.isEnum()) {
                return false;
            }
            ConfigProperty property = source.findProperty(name);
            if (property == null) {
                return false;
            }
            Object value = property.getValue();
            return getEnv().getConverter().canConvert(value, this.valueType);*/
        }

        private String getKeyName(PropertyName name) {
            StringBuilder result = new StringBuilder();
            for (int i = this.root.getElementsLength(); i < name
                    .getElementsLength(); i++) {
                result.append(result.length() != 0 ? "." : "");
                result.append(name.getElement(i));
            }
            return result.toString();
        }

    }
}
