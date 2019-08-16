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
import org.dromara.config.api.property.ConfigProperty;
import org.dromara.config.api.property.ConfigPropertySource;
import org.dromara.config.api.property.PropertyName;
import org.dromara.soul.common.utils.StringUtils;

import java.util.Collection;
import java.util.function.Supplier;

/**
 * IndexedBinder .
 *
 * @author sixh
 */
public abstract class IndexedBinder<T> extends AggregateBinder<T> {

    private final String INDEX_ZERO = "[0]";

    /**
     * Instantiates a new Indexed binder.
     *
     * @param env the env
     */
    IndexedBinder(Binder.Env env) {
        super(env);
    }

    /**
     * Bind indexed elements to the supplied collection.
     *
     * @param root          The name of the property to bind
     * @param target        the target bindable
     * @param elementBinder the binder to use for elements
     * @param aggregateType the aggregate type, may be a collection or an array
     * @param elementType   the element type
     * @param collection    the destination for results
     */
    final void bindIndexed(PropertyName root,
                           BindData<?> target,
                           AggregateElementBinder elementBinder,
                           DataType aggregateType,
                           DataType elementType,
                           IndexedCollectionSupplier collection) {
        ConfigPropertySource source = getEnv().getSource();
        ConfigProperty property = source.findProperty(root);
        if (property != null) {
            bindValue(collection.get(), property.getValue());
        } else {
            bindIndexed(source, root, elementBinder, collection, elementType);
        }
    }

    @SuppressWarnings("unchecked")
    private void bindValue(Collection<Object> collection,
                           Object value) {
        if (value instanceof String && !StringUtils.isNotBlank((String) value)) {
            return;
        }
        if (value instanceof Collection) {
            Collection<Object> elements = (Collection) value;
            collection.addAll(elements);
        }
    }

    private void bindIndexed(ConfigPropertySource source,
                             PropertyName root,
                             AggregateElementBinder elementBinder,
                             IndexedCollectionSupplier collection,
                             DataType elementType) {
        Multimap<String, ConfigProperty> knownIndexedChildren = getKnownIndexedChildren(source, root);
        for (int i = 0; i < Integer.MAX_VALUE; i++) {
            PropertyName name = root
                    .append(i != 0 ? "[" + i + "]" : INDEX_ZERO);
            Object value = elementBinder.bind(name, BindData.of(elementType), getEnv());
            if (value == null) {
                break;
            }
            knownIndexedChildren.removeAll(name.getName());
            collection.get().add(value);
        }
    }

    private Multimap<String, ConfigProperty> getKnownIndexedChildren(ConfigPropertySource source, PropertyName root) {
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

    @Override
    public boolean isAllowRecursiveBinding(Binder.Env source) {
        return source == null;
    }

    /**
     * {@link AggregateBinder.AggregateSupplier AggregateSupplier} for an indexed
     * collection.
     */
    static class IndexedCollectionSupplier
            extends AggregateSupplier<Collection<Object>> {

        /**
         * Instantiates a new Indexed collection supplier.
         *
         * @param supplier the supplier
         */
        IndexedCollectionSupplier(Supplier<Collection<Object>> supplier) {
            super(supplier);
        }

    }

}
