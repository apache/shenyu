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

import org.dromara.config.api.source.PropertyName;
import org.dromara.soul.common.utils.CollectionUtils;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * CollectionBinder .
 * <p>
 * <p>
 * 2019-08-13 21:11
 *
 * @author chenbin sixh
 */
public class CollectionBinder extends IndexedBinder<Collection<Object>> {

    public CollectionBinder(Binder.Env env) {
        super(env);
    }

    @Override
    Object bindAggregate(PropertyName name, BindData<?> target, AggregateElementBinder elementBinder) {
        Class<?> collectionType = (target.getInst() != null ? List.class
                : target.getTypeClass());
        Type aggregateType = collectionType;
        Type elementType = target.getGenerics().length > 0 ? target.getGenerics()[0] : Object.class;
        IndexedCollectionSupplier result = new IndexedCollectionSupplier(
                () -> CollectionUtils.createFactory().create(collectionType, 0));
        bindIndexed(name, target, elementBinder, aggregateType, elementType, result);
        if (result.wasSupplied()) {
            return result.get();
        }
        return null;
    }

    @Override
    Collection<Object> assemble(Supplier<?> inst, Collection<Object> additional) {
        Collection<Object> existingCollection = getExistingIfPossible(inst);
        if (existingCollection == null) {
            return additional;
        }
        try {
            existingCollection.clear();
            existingCollection.addAll(additional);
            return copyIfPossible(existingCollection);
        } catch (UnsupportedOperationException ex) {
            return createNewCollection(additional);
        }
    }

    @SuppressWarnings("unchecked")
    private Collection<Object> getExistingIfPossible(Supplier<?> existing) {
        try {
            return (Collection<Object>) existing.get();
        } catch (Exception ex) {
            return null;
        }
    }

    private Collection<Object> copyIfPossible(Collection<Object> collection) {
        try {
            return createNewCollection(collection);
        } catch (Exception ex) {
            return collection;
        }
    }

    private Collection<Object> createNewCollection(Collection<Object> collection) {
        Collection<Object> result = CollectionUtils.createFactory().create(collection.getClass(), collection.size());
        result.addAll(collection);
        return result;
    }
}
