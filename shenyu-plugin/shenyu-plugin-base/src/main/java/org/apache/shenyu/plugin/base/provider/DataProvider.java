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

package org.apache.shenyu.plugin.base.provider;

import java.util.List;

/**
 * A generic interface for providing data based on a given key.
 *
 * @param <T> the type of data provided by this provider
 */
public interface DataProvider<T> {
    /**
     * Retrieves a list of data items associated with the specified key.
     *
     * @param key the key used to look up the data; its meaning is defined by the implementation
     * @return a list of data items of type {@code T} associated with the given key,
     *         or an empty list if no data is found
     */
    /**
     * Retrieves a list of data items associated with the specified key.
     *
     * @param key the key used to look up the data; its meaning is defined by the implementation
     * @return a list of data items of type {@code T} associated with the given key,
     *         or an empty list if no data is found
     */
    List<T> getData(String key);
}