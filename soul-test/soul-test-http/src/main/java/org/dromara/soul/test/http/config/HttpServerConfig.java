/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.test.http.config;

import org.dromara.soul.test.http.router.SoulTestHttpRouter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.embedded.netty.NettyReactiveWebServerFactory;
import org.springframework.boot.web.reactive.server.ReactiveWebServerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.http.server.reactive.HttpHandler;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;
import reactor.ipc.netty.http.server.HttpServer;
import reactor.ipc.netty.resources.LoopResources;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * HttpServerConfig.
 * @author xiaoyu
 */
@Configuration
public class HttpServerConfig {


    private final Environment environment;

    @Autowired
    public HttpServerConfig(Environment environment) {
        this.environment = environment;
    }

    @Bean
    public RouterFunction<ServerResponse> monoRouterFunction(SoulTestHttpRouter soulTestHttpRouter) {
        return soulTestHttpRouter.routes();
    }

    @Bean
    public HttpServer httpServer(RouterFunction<?> routerFunction) {
        HttpHandler httpHandler = RouterFunctions.toHttpHandler(routerFunction);
        ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(httpHandler);
        HttpServer server = HttpServer.create(
                environment.getProperty("server.address"),
                Integer.valueOf(environment.getProperty("server.port")));
        server.newHandler(adapter);
        return server;
    }

    @Bean
    public ReactiveWebServerFactory reactiveWebServerFactory() {
        NettyReactiveWebServerFactory factory = new NettyReactiveWebServerFactory();
        factory.addServerCustomizers(builder -> {
            builder.loopResources(LoopResources.create("webflux-http", 8, true));
        });
        return factory;
    }


    @Bean
    public Scheduler scheduler() {
        ExecutorService threadPool = Executors.newFixedThreadPool(100);
        return Schedulers.fromExecutor(threadPool);
    }
}
