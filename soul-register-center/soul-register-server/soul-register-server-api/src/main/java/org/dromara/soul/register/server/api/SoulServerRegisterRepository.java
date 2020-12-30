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

package org.dromara.soul.register.server.api;

import org.dromara.soul.register.common.config.SoulRegisterCenterConfiguration;
import org.dromara.soul.register.server.api.listener.DataChangedEventListener;

/**
 * Soul client register repository.
 */
public interface SoulServerRegisterRepository {
    
    /**
     * Path separator.
     */
    String PATH_SEPARATOR = "/";
    
    /**
     * Dot separator.
     */
    String DOT_SEPARATOR = ".";
    
    /**
     * Init.
     *
     * @param config the config
     */
    default void init(SoulRegisterCenterConfiguration config) {
    }
    
    /**
     * Watch key or path of governance server.
     *
     * @param key key of data
     * @param listener data changed event listener
     */
    void watch(String key, DataChangedEventListener listener);
    
    /**
     * Close.
     */
    default void close() {
    }
}
