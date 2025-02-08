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

package org.apache.shenyu.client.core.shutdown;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * HookManager.
 */
public final class ShutdownHookManager {

    private static final ShutdownHookManager MGR = new ShutdownHookManager();

    private static final Logger LOG = LoggerFactory.getLogger(ShutdownHookManager.class);

    private static String hookName = "ShenyuClientShutdownHook";

    private final Set<HookEntry> hooks =
            Collections.synchronizedSet(new HashSet<>());

    private final AtomicBoolean shutdownInProgress = new AtomicBoolean(false);

    private ShutdownHookManager() {

    }

    static {
        Runtime.getRuntime().addShutdownHook(
                new Thread(() -> {
                    MGR.shutdownInProgress.set(true);
                    for (Runnable hook : MGR.getShutdownHooksInOrder()) {
                        try {
                            hook.run();
                        } catch (Throwable ex) {
                            LOG.error(ex.getMessage(), ex);
                        }
                    }
                }, getHookName())
        );
        LOG.info("Add hook {}", getHookName());
    }

    /**
     * Return <code>ShutdownHookManager</code> singleton.
     *
     * @return <code>ShutdownHookManager</code> singleton.
     */
    public static ShutdownHookManager get() {
        return MGR;
    }

    /**
     * Returns the list of shutdownHooks in order of execution,
     * Highest priority first.
     *
     * @return the list of shutdownHooks in order of execution.
     */
    List<Runnable> getShutdownHooksInOrder() {
        List<HookEntry> list;
        synchronized (MGR.hooks) {
            list = new ArrayList<>(MGR.hooks);
        }
        list.sort((o1, o2) -> o2.priority - o1.priority);
        List<Runnable> ordered = new ArrayList<>();
        list.forEach(entry -> ordered.add(entry.hook));
        return ordered;
    }

    /**
     * Adds a shutdownHook with default priority zero, the higher the priority
     * the earlier will run. ShutdownHooks with same priority run
     * in a non-deterministic order.
     *
     * @param shutdownHook shutdownHook <code>Runnable</code>
     */
    public void addShutdownHook(final Runnable shutdownHook) {
        if (Objects.isNull(shutdownHook)) {
            throw new IllegalArgumentException("shutdownHook cannot be NULL");
        }
        if (shutdownInProgress.get()) {
            throw new IllegalStateException("Shutdown in progress, cannot add a shutdownHook");
        }
        hooks.add(new HookEntry(shutdownHook, 0));
    }

    /**
     * Adds a shutdownHook with a priority, the higher the priority
     * the earlier will run. ShutdownHooks with same priority run
     * in a non-deterministic order.
     *
     * @param shutdownHook shutdownHook <code>Runnable</code>
     * @param priority     priority of the shutdownHook.
     */
    public void addShutdownHook(final Runnable shutdownHook, final int priority) {
        if (Objects.isNull(shutdownHook)) {
            throw new IllegalArgumentException("shutdownHook cannot be NULL");
        }
        if (shutdownInProgress.get()) {
            throw new IllegalStateException("Shutdown in progress, cannot add a shutdownHook");
        }
        hooks.add(new HookEntry(shutdownHook, priority));
    }

    /**
     * Removes a shutdownHook.
     * @param shutdownHook shutdownHook to remove.
     * @return TRUE if the shutdownHook was registered and removed,FALSE otherwise.
     */
    public boolean removeShutdownHook(final Runnable shutdownHook) {
        if (shutdownInProgress.get()) {
            throw new IllegalStateException("Shutdown in progress, cannot remove a shutdownHook");
        }
        return hooks.remove(new HookEntry(shutdownHook, 0));
    }

    /**
     * Indicates if a shutdownHook is registered or not.
     *
     * @param shutdownHook shutdownHook to check if registered.
     * @return TRUE/FALSE depending if the shutdownHook is is registered.
     */
    public boolean hasShutdownHook(final Runnable shutdownHook) {
        return hooks.contains(new HookEntry(shutdownHook, 0));
    }

    /**
     * Indicates if shutdown is in progress or not.
     *
     * @return TRUE if the shutdown is in progress, otherwise FALSE.
     */
    public boolean isShutdownInProgress() {
        return shutdownInProgress.get();
    }

    /**
     * clear all registered shutdownHooks.
     */
    public void clearShutdownHooks() {
        hooks.clear();
    }

    /**
     * Returns client shutdown hook name.
     *
     * @return client shutdown hook name
     */
    public static String getHookName() {
        return hookName;
    }

    /**
     * clear client shutdown hook name.
     */
    public static void clearHookName() {
        hookName = null;
    }

    /**
     * Private structure to store ShutdownHook and its priority.
     */
    private static class HookEntry {

        private final Runnable hook;

        private final int priority;

        HookEntry(final Runnable hook, final int priority) {
            this.hook = hook;
            this.priority = priority;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (Objects.isNull(o) || getClass() != o.getClass()) {
                return false;
            }
            HookEntry hookEntry = (HookEntry) o;
            return priority == hookEntry.priority && Objects.equals(hook, hookEntry.hook);
        }

        @Override
        public int hashCode() {
            return Objects.hash(hook, priority);
        }
    }

}
