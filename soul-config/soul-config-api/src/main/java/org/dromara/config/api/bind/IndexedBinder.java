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

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import org.dromara.config.api.ConfigException;
import org.dromara.config.api.source.ConfigProperty;
import org.dromara.config.api.source.ConfigPropertySource;
import org.dromara.config.api.source.PropertyName;
import org.dromara.soul.common.utils.StringUtils;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.function.Supplier;

/**
 * IndexedBinder .
 * <p>
 * <p>
 * 2019-08-13 21:10
 *
 * @author chenbin sixh
 */
public abstract class IndexedBinder<T> extends AggregateBinder<T> {

    private static final String INDEX_ZERO = "[0]";

    public IndexedBinder(Binder.Env env) {
        super(env);
    }

    @Override
    protected boolean isAllowRecursiveBinding(ConfigPropertySource source) {
        return source == null;
    }

    /**
     * Bind indexed elements to the supplied collection.
     *
     * @param name          The name of the property to bind
     * @param target        the target bindable
     * @param elementBinder the binder to use for elements
     * @param aggregateType the aggregate type, may be a collection or an array
     * @param elementType   the element type
     * @param result        the destination for results
     */
    protected final void bindIndexed(PropertyName name, BindData<?> target,
                                     AggregateElementBinder elementBinder, Type aggregateType,
                                     Type elementType, IndexedCollectionSupplier result) {
        for (ConfigPropertySource source : getEnv().getSources()) {
            bindIndexed(source, name, target, elementBinder, result, aggregateType,
                    elementType);
            if (result.wasSupplied() && result.get() != null) {
                return;
            }
        }
    }

    private void bindIndexed(ConfigPropertySource source,
                             PropertyName root, BindData<?> target,
                             AggregateElementBinder elementBinder,
                             IndexedCollectionSupplier collection,
                             Type aggregateType, Type elementType) {
        ConfigProperty property = source.findProperty(root);
        if (property != null) {
            bindValue(target, collection.get(), aggregateType, elementType,
                    property.getValue());
        } else {
            bindIndexed(source, root, elementBinder, collection, elementType);
        }
    }

    private void bindValue(BindData<?> target, Collection<Object> collection,
                           Type aggregateType, Type elementType, Object value) {
        if (value instanceof String && !StringUtils.isNotBlank((String) value)) {
            return;
        }
        Object aggregate = convert(value, aggregateType);
/*        Type collectionType = ResolvableType
                .forClassWithGenerics(collection.getClass(), elementType);*/
        Collection<Object> elements = convert(aggregate, null);
        collection.addAll(elements);
    }


    private void bindIndexed(ConfigPropertySource source,
                             PropertyName root, AggregateElementBinder elementBinder,
                             IndexedCollectionSupplier collection, Type elementType) {
        Multimap<String, ConfigProperty> knownIndexedChildren = getKnownIndexedChildren(source, root);
        for (int i = 0; i < Integer.MAX_VALUE; i++) {
            PropertyName name = root
                    .append(i != 0 ? "[" + i + "]" : INDEX_ZERO);
            Object value = elementBinder.bind(name, BindData.of(elementType), source);
            if (value == null) {
                break;
            }
            knownIndexedChildren.removeAll(name.getName());
            collection.get().add(value);
        }
        assertNoUnboundChildren(knownIndexedChildren);
    }

    private Multimap<String, ConfigProperty> getKnownIndexedChildren(
            ConfigPropertySource source, PropertyName root) {
        Multimap<String, ConfigProperty> children = ArrayListMultimap.create();
        source.stream().filter(e -> e.isAncestorOf(root)).forEach(name -> {
            if (name.isLastElementIndexed()) {
                String key = name.getLastElement();
                ConfigProperty property = source.findProperty(name);
                children.put(key, property);
            }
        });
        return children;
    }

    private void assertNoUnboundChildren(
            Multimap<String, ConfigProperty> children) {
        if (!children.isEmpty()) {
            throw new ConfigException("assertNoUnboundChildren");
        }
    }

    private <C> C convert(Object value, Type type) {
//        value = getEnv().getPlaceholdersResolver().resolvePlaceholders(value);
//        return getContext().getConverter().convert(value, type, annotations);
        return (C) value;
    }

    /**
     * {@link AggregateBinder.AggregateSupplier AggregateSupplier} for an indexed
     * collection.
     */
    protected static class IndexedCollectionSupplier
            extends AggregateSupplier<Collection<Object>> {

        public IndexedCollectionSupplier(Supplier<Collection<Object>> supplier) {
            super(supplier);
        }

    }
}
