/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.agent.matcher;

import net.bytebuddy.description.type.TypeDefinition;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.matcher.ElementMatcher;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * It can return has parent type class matcher.
 */
public class HasParentTypeMatcher extends ElementMatcher.Junction.AbstractBase<TypeDescription> {

    private final ElementMatcher<TypeDescription.Generic> matcher;

    private final boolean interfacesOnly;

    /**
     * Creates a new matcher for a parent type.
     *
     * @param matcher The matcher to apply to any parent type of the matched type.
     */
    public HasParentTypeMatcher(
            final ElementMatcher<TypeDescription.Generic> matcher, final boolean interfacesOnly) {
        this.matcher = matcher;
        this.interfacesOnly = interfacesOnly;
    }

    @Override
    public boolean matches(final TypeDescription target) {
        Set<TypeDescription> checkedInterfaces = new HashSet<>(8);
        TypeDefinition typeDefinition = target;
        while (typeDefinition != null) {
            if (((!interfacesOnly || typeDefinition.isInterface())
                    && matcher.matches(typeDefinition.asGenericType()))
                    || hasInterface(typeDefinition, checkedInterfaces)) {
                return true;
            }
            typeDefinition = safeGetParentClass(typeDefinition);
        }
        return false;
    }

    /**
     * Matches a type's interfaces against the provided matcher.
     *
     * @param typeDefinition The type for which to check all implemented interfaces.
     * @param checkedInterfaces The interfaces that have already been checked.
     * @return {@code true} if any interface matches the supplied matcher.
     */
    private boolean hasInterface(
            final TypeDefinition typeDefinition, final Set<TypeDescription> checkedInterfaces) {
        for (TypeDefinition interfaceType : safeGetInterfaces(typeDefinition)) {
            TypeDescription erasure = safeAsErasure(interfaceType);
            if (erasure != null) {
                if (checkedInterfaces.add(interfaceType.asErasure())
                        && (matcher.matches(interfaceType.asGenericType())
                        || hasInterface(interfaceType, checkedInterfaces))) {
                    return true;
                }
            }
        }
        return false;
    }

    private static Iterable<TypeDefinition> safeGetInterfaces(final TypeDefinition typeDefinition) {
        return new SafeInterfaceIterator(typeDefinition);
    }

    static TypeDefinition safeGetParentClass(final TypeDefinition typeDefinition) {
        try {
            return typeDefinition.getSuperClass();
        } catch (Throwable e) {
            return null;
        }
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (!(obj instanceof HasParentTypeMatcher)) {
            return false;
        }
        HasParentTypeMatcher other = (HasParentTypeMatcher) obj;
        return matcher.equals(other.matcher);
    }

    @Override
    public int hashCode() {
        return matcher.hashCode();
    }

    static TypeDescription safeAsErasure(final TypeDefinition typeDefinition) {
        try {
            return typeDefinition.asErasure();
        } catch (Throwable e) {
            return null;
        }
    }

    private static final class SafeInterfaceIterator
            implements Iterator<TypeDefinition>, Iterable<TypeDefinition> {

        private final Iterator<TypeDescription.Generic> it;

        private TypeDefinition next;

        private SafeInterfaceIterator(final TypeDefinition typeDefinition) {
            Iterator<TypeDescription.Generic> it = null;
            try {
                it = typeDefinition.getInterfaces().iterator();
            } catch (Throwable ignored) {
            }
            this.it = it;
        }

        @Override
        public boolean hasNext() {
            if (null != it && it.hasNext()) {
                try {
                    next = it.next();
                    return true;
                } catch (Throwable e) {
                    return false;
                }
            }
            return false;
        }

        @Override
        public TypeDefinition next() {
            return next;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }

        @Override
        public Iterator<TypeDefinition> iterator() {
            return this;
        }
    }

}

