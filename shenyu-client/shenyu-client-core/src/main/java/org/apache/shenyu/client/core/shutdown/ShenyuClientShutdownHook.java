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

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;

import java.lang.reflect.Field;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Properties;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Shenyu client shutdown hook.
 */
@Slf4j
public class ShenyuClientShutdownHook {

    private static String hookNamePrefix = "ShenyuClientShutdownHook";

    private static AtomicInteger hookId = new AtomicInteger(0);

    private static Properties props;

    private static AtomicBoolean delay = new AtomicBoolean(false);

    private static IdentityHashMap<Thread, Thread> delayHooks = new IdentityHashMap<>();

    private static IdentityHashMap<Thread, Thread> delayedHooks = new IdentityHashMap<>();

    /**
     * Add shenyu client shutdown hook.
     *
     * @param result ShenyuClientRegisterRepository
     * @param props  Properties
     */
    public static void set(final ShenyuClientRegisterRepository result, final Properties props) {
        String name = hookNamePrefix + "-" + hookId.incrementAndGet();
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            result.close();
        }, name));
        log.info("Add hook {}", name);
        ShenyuClientShutdownHook.props = props;
    }

    /**
     * Delay other shutdown hooks.
     */
    public static void delayOtherHooks() {
        if (!delay.compareAndSet(false, true)) {
            return;
        }
        TakeoverOtherHooksThread thread = new TakeoverOtherHooksThread();
        thread.start();
    }

    /**
     * Delay other shutdown hooks thread.
     */
    private static class TakeoverOtherHooksThread extends Thread {
        @SneakyThrows
        @Override
        public void run() {
            int shutdownWaitTime = Integer.parseInt(props.getProperty("shutdownWaitTime", "3000"));
            int delayOtherHooksExecTime = Integer.parseInt(props.getProperty("delayOtherHooksExecTime", "2000"));
            Class<?> clazz = Class.forName(props.getProperty("applicationShutdownHooksClassName", "java.lang.ApplicationShutdownHooks"));
            Field field = clazz.getDeclaredField(props.getProperty("applicationShutdownHooksFieldName", "hooks"));
            field.setAccessible(true);
            IdentityHashMap<Thread, Thread> hooks = (IdentityHashMap<Thread, Thread>) field.get(clazz);
            long s = System.currentTimeMillis();
            while (System.currentTimeMillis() - s < delayOtherHooksExecTime) {
                for (Iterator<Thread> iterator = hooks.keySet().iterator(); iterator.hasNext();) {
                    Thread hook = iterator.next();
                    if (hook.getName().startsWith(hookNamePrefix)) {
                        continue;
                    }
                    if (delayHooks.containsKey(hook) || delayedHooks.containsKey(hook)) {
                        continue;
                    }
                    Thread delayHook = new Thread(() -> {
                        log.info("sleep {}ms", shutdownWaitTime);
                        try {
                            TimeUnit.MILLISECONDS.sleep(shutdownWaitTime);
                        } catch (InterruptedException ignore) { }
                        hook.run();
                    }, hook.getName());
                    delayHooks.put(delayHook, delayHook);
                    iterator.remove();
                }

                for (Iterator<Thread> iterator = delayHooks.keySet().iterator(); iterator.hasNext();) {
                    Thread delayHook = iterator.next();
                    Runtime.getRuntime().addShutdownHook(delayHook);
                    delayedHooks.put(delayHook, delayHook);
                    iterator.remove();
                    log.info("hook {} will sleep {}ms when it start", delayHook.getName(), shutdownWaitTime);
                }
                TimeUnit.MILLISECONDS.sleep(100);
            }

            hookNamePrefix = null;
            hookId = null;
            props = null;
            delayHooks = null;
            delayedHooks = null;
        }
    }
}
