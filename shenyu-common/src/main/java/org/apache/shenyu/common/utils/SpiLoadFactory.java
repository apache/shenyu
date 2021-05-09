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

package org.apache.shenyu.common.utils;

import java.util.Iterator;
import java.util.ServiceLoader;

/**
 * SpiLoadFactory.
 */
public class SpiLoadFactory {

    /**
     * Load first s.
     *
     * @param <S>   the type parameter
     * @param clazz the clazz
     * @return the s
     */
    public static <S> S loadFirst(final Class<S> clazz) {
        final ServiceLoader<S> loader = loadAll(clazz);
        final Iterator<S> iterator = loader.iterator();
        if (!iterator.hasNext()) {
            throw new IllegalStateException(String.format(
                    "No implementation defined in /META-INF/services/%s, please check whether the file exists and has the right implementation class!",
                    clazz.getName()));
        }
        return iterator.next();
    }

    /**
     * Load all service loader.
     *
     * @param <S>   the type parameter
     * @param clazz the clazz
     * @return the service loader
     */
    public static <S> ServiceLoader<S> loadAll(final Class<S> clazz) {
        return ServiceLoader.load(clazz);
    }
}
