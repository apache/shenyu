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

package org.apache.shenyu.examples.http.config;

import org.apache.shenyu.examples.http.router.ShenyuTestHttpRouter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * HttpServerConfig.
 */
@Configuration
public class HttpServerConfig {

    /**
     * define a bean with name monoRouterFunction for test.
     * @param shenyuTestHttpRouter shenyuTestHttpRouter
     * @return route functions
     */
    @Bean
    public RouterFunction<ServerResponse> monoRouterFunction(final ShenyuTestHttpRouter shenyuTestHttpRouter) {
        return shenyuTestHttpRouter.routes();
    }

    /**
     * define scheduler bean.
     * @return Scheduler bean.
     */
    @Bean
    public Scheduler scheduler() {
        ExecutorService threadPool = new ThreadPoolExecutor(100, 100,
            0L, TimeUnit.MILLISECONDS,
            new LinkedBlockingQueue<>(),
            runnable -> {
                Thread thread = new Thread(runnable, "http-exe");
                thread.setDaemon(false);
                if (thread.getPriority() != Thread.NORM_PRIORITY) {
                    thread.setPriority(Thread.NORM_PRIORITY);
                }
                return thread;
            });
        return Schedulers.fromExecutor(threadPool);
    }
}
