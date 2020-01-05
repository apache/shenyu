/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.dromara.soul.register.api;

import com.google.common.collect.Maps;
import org.dromara.soul.common.concurrent.SoulThreadFactory;
import org.dromara.soul.common.http.URL;
import org.dromara.soul.common.timer.HashedWheelTimer;
import org.dromara.soul.common.timer.Timeout;
import org.dromara.soul.common.timer.Timer;
import org.dromara.soul.common.timer.TimerTask;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * FailbackRegistry .
 *
 * @author sixh
 */
public abstract class FailbackRegistry extends AbstractRegistry {

    private Logger logger = LoggerFactory.getLogger(FailbackRegistry.class);

    private final Map<URL, FailbackTask> failbackRegister = Maps.newConcurrentMap();

    private final Map<URL, FailbackTask> failbackSubscribe = Maps.newConcurrentMap();

    private static final HashedWheelTimer TIMER = new HashedWheelTimer(SoulThreadFactory.create("soul.failback.register"));
    /**
     * retry exec time.
     */
    private final Long retryTime = TimeUnit.SECONDS.toMillis(5);

    /**
     * Instantiates a new Abstract registry.
     *
     * @param url the url.
     */
    public FailbackRegistry(URL url) {
        super(url);
    }

    @Override
    public void register(URL url) {
        removeFailbackRegister(url);
        try {
            super.register(url);
            doRegister(url);
        } catch (Throwable t) {
            addFailbackRegister(url, new FailbackTask(url, url1 -> {
                doRegister(url1);
                failbackRegister.remove(url1);
            }, retryTime));
        }
    }

    @Override
    public void unregister(URL url) {
        removeFailbackRegister(url);
        try {
            super.unregister(url);
            doUnRegister(url);
        } catch (Throwable t) {
            addFailbackRegister(url, new FailbackTask(url, url1 -> {
                doUnRegister(url1);
                failbackRegister.remove(url1);
            }, retryTime));
        }
    }

    @Override
    public void subscribe(URL url, RegisterNotifyListener listener) {
        removeFailbackSubscribe(url);
        try {
            super.subscribe(url, listener);
            doSubscribe(url);
        } catch (Throwable t) {
            addFailbackSubscribe(url, new FailbackTask(url, url1 -> {
                doSubscribe(url1);
                failbackSubscribe.remove(url1);
            }, retryTime));
        }
    }

    @Override
    public void unsubscribe(URL url) {
        removeFailbackSubscribe(url);
        try {
            super.unsubscribe(url);
            doUnSubscribe(url);
        } catch (Throwable t) {
            addFailbackSubscribe(url, new FailbackTask(url, url1 -> {
                doUnSubscribe(url1);
                failbackSubscribe.remove(url1);
            }, retryTime));
        }
    }

    private void addFailbackRegister(URL url, FailbackTask task) {
        FailbackTask failbackTask = failbackRegister.get(url);
        if (failbackTask != null) {
            return;
        }
        failbackRegister.putIfAbsent(url, task);
        TIMER.newTimeout(task, retryTime, TimeUnit.MILLISECONDS);
    }

    private void removeFailbackRegister(URL url) {
        FailbackTask remove = failbackRegister.remove(url);
        if (remove != null) {
            remove.cancel();
        }
    }

    private void addFailbackSubscribe(URL url, FailbackTask task) {
        FailbackTask failbackTask = failbackRegister.get(url);
        if (failbackTask != null) {
            return;
        }
        failbackSubscribe.putIfAbsent(url, task);
        TIMER.newTimeout(task, retryTime, TimeUnit.MILLISECONDS);
    }

    private void removeFailbackSubscribe(URL url) {
        FailbackTask remove = failbackSubscribe.remove(url);
        if (remove != null) {
            remove.cancel();
        }
    }

    protected void recover() {
        Set<URL> registered = getRegistered();
        if (!registered.isEmpty()) {
            if (logger.isInfoEnabled()) {
                logger.info("registered : {}", registered);
            }
            registered.forEach(e -> addFailbackSubscribe(e, new FailbackTask(e,
                    url -> {
                        doRegister(url);
                        failbackRegister.remove(url);
                    }, retryTime)));
        }
        Map<URL, RegisterNotifyListener> subscribed = getSubscribed();
        if (!subscribed.isEmpty()) {
            if (logger.isInfoEnabled()) {
                logger.info("subscribed: {}", subscribed);
            }
            subscribed.forEach((k, v) -> addFailbackSubscribe(k, new FailbackTask(k, url -> {
                doSubscribe(url);
                failbackSubscribe.remove(url);
            }, retryTime)));
        }
    }

    /**
     * register template method.
     *
     * @param url url.
     */
    protected abstract void doRegister(URL url);

    /**
     * Do un register.
     *
     * @param url the url
     */
    protected abstract void doUnRegister(URL url);

    /**
     * Do subscribe.
     *
     * @param url the url
     */
    protected abstract void doSubscribe(URL url);

    /**
     * Do un subscribe.
     *
     * @param url the url
     */
    protected abstract void doUnSubscribe(URL url);

    /**
     * The type Failback task.
     */
    static class FailbackTask implements TimerTask {

        private FailbackRunner runner;

        private URL url;

        private Long tick;

        private volatile boolean cancel = false;

        /**
         * Instantiates a new Failback task.
         *
         * @param url    the url
         * @param runner the runner
         * @param tick   the tick
         */
        FailbackTask(URL url, FailbackRunner runner, Long tick) {
            this.runner = runner;
            this.url = url;
            this.tick = tick;
        }

        /**
         * Cancel.
         */
        void cancel() {
            this.cancel = true;
        }

        /**
         * Is cancel boolean.
         *
         * @return the boolean
         */
        private boolean isCancel() {
            return cancel;
        }

        @Override
        public void run(Timeout timeout) {
            if (timeout.isCancelled() || timeout.timer().isStop() || isCancel()) {
                // other thread cancel this timeout or stop the timer.
                return;
            }
            try {
                runner.apply(url);
            } catch (Throwable e) {
                replay(timeout, tick);
            }
        }

        private void replay(Timeout timeout, long tick) {
            Timer timer = timeout.timer();
            if (timer.isStop() || timeout.isCancelled() || isCancel()) {
                return;
            }
            timer.newTimeout(this, tick, TimeUnit.MILLISECONDS);
        }
    }

    /**
     * The interface Failback runner.
     */
    @FunctionalInterface
    interface FailbackRunner {
        /**
         * Apply.
         *
         * @param url the url
         */
        void apply(URL url);
    }
}
