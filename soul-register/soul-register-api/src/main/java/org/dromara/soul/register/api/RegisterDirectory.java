/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.register.api;

import java.util.HashSet;
import java.util.Set;
import org.dromara.soul.common.extension.SPI;
import org.dromara.soul.register.api.path.Path;

/**
 * RegisterDirectory
 * 1. Listen to the service interface for the relevant registration processing.
 *
 * @author sixh
 */
@SPI
public abstract class RegisterDirectory implements HealthCheck {

    private Set<RegisterDirectoryListener> listeners = new HashSet<>();

    /**
     * Listener.
     *
     * @param listener the listener.
     */
    public void listener(RegisterDirectoryListener listener) {
        listeners.add(listener);
    }

    /**
     * Redress.
     *
     * @param path the path.
     */
    protected void redress(Path path) {
        listeners.forEach(listener -> listener.apply(path));
    }
}
