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

package org.apache.shenyu.plugin.jwt.exception;

/**
 * Throwing wrap.
 * Handle exception, and return null.
 * Not all exceptions must be try catch, throw upwards, return null, you can use if to judge.
 */
@FunctionalInterface
public interface ThrowingFunction<T> {

    /**
     * apply function.
     *
     * @return T
     * @throws Exception any error
     */
    T apply() throws Exception;

    /**
     * operation function.
     * If an exception occurs, it will directly return a null.
     *
     * @param function apply function.
     * @param <T> t
     * @return T
     */
    static <T> T wrap(ThrowingFunction<T> function) {
        try {
            return function.apply();
        } catch (Exception e) {
            return null;
        }
    }
}
