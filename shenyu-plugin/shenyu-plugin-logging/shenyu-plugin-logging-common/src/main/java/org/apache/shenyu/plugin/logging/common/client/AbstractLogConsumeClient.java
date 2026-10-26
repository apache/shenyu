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

package org.apache.shenyu.plugin.logging.common.client;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;
import org.springframework.util.ObjectUtils;

import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/**
 * AbstractLogConsumeClient.
 */
public abstract class AbstractLogConsumeClient<T extends GenericGlobalConfig, L extends ShenyuRequestLog>
        implements LogConsumeClient<T, L> {

    protected static final Logger LOG = LoggerFactory.getLogger(AbstractLogConsumeClient.class);

    private final AtomicBoolean isStarted = new AtomicBoolean(false);

    private final AtomicReference<Thread> closeThread = new AtomicReference<>();

    /**
     * initClient0.
     *
     * @param config config
     * @return true if the client was initialized successfully
     */
    public abstract boolean initClient0(@NonNull T config);

    /**
     * consume0.
     *
     * @param logs logs
     * @throws Exception error
     */
    public abstract void consume0(@NonNull List<L> logs) throws Exception;

    /**
     * close0.
     *
     * @throws Exception error
     */
    public abstract void close0() throws Exception;

    @Override
    public void initClient(final T config) {
        if (isStarted.get()) {
            this.close();
        }
        if (ObjectUtils.isEmpty(config)) {
            LOG.error("{} config is null, client not init.", this.getClass().getSimpleName());
            return;
        }
        boolean initialized = this.initClient0(config);
        isStarted.set(initialized);
        if (initialized) {
            closeThread.set(new Thread(this::close));
            Runtime.getRuntime().addShutdownHook(closeThread.get());
        }
    }

    @Override
    public void close() {
        this.tryRemoveShutdownHook();
        if (isStarted.compareAndSet(true, false)) {
            try {
                this.close0();
            } catch (Exception e) {
                LOG.error("{} close error.", this.getClass().getSimpleName(), e);
            }
        }
    }

    /**
     * Remove the registered shutdown hook. When the JVM is already shutting down,
     * the hook thread is running this {@code close()} itself, so removal throws
     * {@link IllegalStateException} and is neither possible nor needed; cleanup
     * must still proceed to {@code close0()}.
     */
    protected void tryRemoveShutdownHook() {
        final Thread hook = closeThread.get();
        if (ObjectUtils.isEmpty(hook)) {
            return;
        }
        try {
            removeHook(hook);
        } catch (IllegalStateException e) {
            LOG.debug("{} skips shutdown-hook removal: {}", this.getClass().getSimpleName(), e.getMessage());
        }
    }

    /**
     * Single point that talks to {@link Runtime}; protected so tests can simulate
     * the {@code IllegalStateException("Shutdown in progress")} thrown when the JVM
     * is already shutting down and the hook thread is running {@code close()} itself.
     *
     * @param hook the shutdown hook registered at init
     */
    protected void removeHook(final Thread hook) {
        Runtime.getRuntime().removeShutdownHook(hook);
    }

    @Override
    public void consume(final List<L> logs) throws Exception {
        if (CollectionUtils.isEmpty(logs) || !isStarted.get()) {
            return;
        }
        this.consume0(logs);
    }
}
