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

package org.apache.shenyu.springboot.starter.k8s;

import io.kubernetes.client.extended.controller.ControllerManager;
import org.springframework.context.SmartLifecycle;

import java.util.List;
import java.util.concurrent.ExecutorService;

/**
 * Starts and stops the {@link ControllerManager}s through the Spring lifecycle instead of
 * submitting them as a side effect of bean creation, so controllers only start once the
 * context is fully refreshed and shut down cleanly on close.
 */
final class ControllerManagerLifecycle implements SmartLifecycle {

    private final List<ControllerManager> controllerManagers;

    private final ExecutorService executorService;

    private volatile boolean running;

    ControllerManagerLifecycle(final List<ControllerManager> controllerManagers,
                               final ExecutorService executorService) {
        this.controllerManagers = controllerManagers;
        this.executorService = executorService;
    }

    @Override
    public void start() {
        controllerManagers.forEach(executorService::submit);
        running = true;
    }

    @Override
    public void stop() {
        controllerManagers.forEach(ControllerManager::shutdown);
        running = false;
    }

    @Override
    public boolean isRunning() {
        return running;
    }
}
