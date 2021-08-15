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

/**
 * Erasure matcher.
 */
public class SafeErasureMatcher<T extends TypeDefinition> extends ElementMatcher.Junction.AbstractBase<T> {

    /** The matcher to apply to the raw type of the matched element. */
    private final ElementMatcher<TypeDescription> matcher;

    /**
     * Creates a new erasure matcher.
     *
     * @param matcher The matcher to apply to the raw type.
     */
    public SafeErasureMatcher(final ElementMatcher<TypeDescription> matcher) {
        this.matcher = matcher;
    }

    @Override
    public boolean matches(final T target) {
        TypeDescription erasure = safeAsErasure(target);
        if (erasure == null) {
            return false;
        } else {
            // We would like matcher exceptions to propagate
            return matcher.matches(erasure);
        }
    }

    static TypeDescription safeAsErasure(final TypeDefinition typeDefinition) {
        try {
            return typeDefinition.asErasure();
        } catch (Throwable e) {
            return null;
        }
    }

    @Override
    public String toString() {
        return "safeErasure(" + matcher + ")";
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (!(obj instanceof SafeErasureMatcher)) {
            return false;
        }
        SafeErasureMatcher<?> other = (SafeErasureMatcher<?>) obj;
        return matcher.equals(other.matcher);
    }

    @Override
    public int hashCode() {
        return matcher.hashCode();
    }
}

