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

package org.apache.shenyu.client.core.register.matcher;

import java.util.Objects;

public interface Matcher<T> {

    /**
     * Evaluates this Matcher on the given element.
     *
     * @param element element
     * @return true if the element argument matches the Matcher, otherwise false
     */
    boolean match(T element);

    /**
     * Returns a composed matcher that represents a short-circuiting logical AND of this matcher and another.
     * When evaluating the composed matcher, if this matcher is false, then the other matcher is not evaluated.
     *
     * @param other a matcher that will be logically-ANDed with this matcher
     * @return a composed matcher
     */
    default Matcher<T> and(Matcher<? super T> other) {
        Objects.requireNonNull(other);
        return t -> match(t) && other.match(t);
    }

    /**
     * Returns a composed matcher that represents a short-circuiting logical OR of this matcher and another.
     * When evaluating the composed matcher, if this matcher is true, then the other matcher is not evaluated.
     *
     * @param other a predicate that will be logically-ORed with this matcher
     * @return a composed matcher that represents the short-circuiting logical OR of this matcher and the other matcher
     */
    default Matcher<T> or(Matcher<? super T> other) {
        Objects.requireNonNull(other);
        return t -> match(t) || other.match(t);
    }

    /**
     * Returns a matcher that represents the logical negation of this matcher.
     *
     * @return a matcher that represents the logical negation of this matcher
     */
    default Matcher<T> negate() {
        return t -> !match(t);
    }

    /**
     * Returns a matcher that is the negation of the supplied matcher.
     * This is accomplished by returning result of the calling target.negate().
     *
     * @param target target – matcher to negate
     * @param <T>    the type of arguments to the specified matcher
     * @return a matcher that negates the results of the supplied matcher
     */
    static <T> Matcher<T> not(Matcher<? super T> target) {
        Objects.requireNonNull(target);
        return (Matcher<T>) target.negate();
    }
}
