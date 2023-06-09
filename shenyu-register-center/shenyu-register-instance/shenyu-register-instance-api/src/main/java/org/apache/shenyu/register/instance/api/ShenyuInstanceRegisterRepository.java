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

package org.apache.shenyu.register.instance.api;

import org.apache.shenyu.register.instance.api.config.RegisterConfig;
import org.apache.shenyu.register.instance.api.entity.InstanceEntity;
import org.apache.shenyu.register.instance.api.watcher.WatcherListener;
import org.apache.shenyu.spi.SPI;

import java.util.Collections;
import java.util.List;

/**
 * Shenyu instance register repository.
 */
@SPI
public interface ShenyuInstanceRegisterRepository {

    /**
     * Init.
     *
     * @param config the config
     */
    default void init(RegisterConfig config) {
    }
    
    /**
     * Persist instance.
     *
     * @param instance instance
     */
    void persistInstance(InstanceEntity instance);

    /**
     * selectInstancesAndWatcher.
     *
     * @param selectKey selectKey
     * @param watcherListener watcherListener
     * @return {@link List}
     */
    default List<InstanceEntity> selectInstancesAndWatcher(final String selectKey, final WatcherListener watcherListener) {
        return Collections.emptyList();
    }

    /**
     * Close.
     */
    default void close() {
    }
}
