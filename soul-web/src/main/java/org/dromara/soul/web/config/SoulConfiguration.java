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

package org.dromara.soul.web.config;

import org.dromara.soul.web.cache.UpstreamCacheManager;
import org.dromara.soul.web.cache.ZookeeperCacheManager;
import org.dromara.soul.web.disruptor.publisher.SoulEventPublisher;
import org.dromara.soul.web.filter.BodyWebFilter;
import org.dromara.soul.web.filter.ParamWebFilter;
import org.dromara.soul.web.filter.TimeWebFilter;
import org.dromara.soul.web.filter.WebSocketWebFilter;
import org.dromara.soul.web.handler.SoulHandlerMapping;
import org.dromara.soul.web.handler.SoulWebHandler;
import org.dromara.soul.web.plugin.SoulPlugin;
import org.dromara.soul.web.plugin.after.MonitorPlugin;
import org.dromara.soul.web.plugin.after.ResponsePlugin;
import org.dromara.soul.web.plugin.before.GlobalPlugin;
import org.dromara.soul.web.plugin.before.SignPlugin;
import org.dromara.soul.web.plugin.before.WafPlugin;
import org.dromara.soul.web.plugin.dubbo.GenericParamService;
import org.dromara.soul.web.plugin.dubbo.GenericParamServiceImpl;
import org.dromara.soul.web.plugin.function.DividePlugin;
import org.dromara.soul.web.plugin.function.RateLimiterPlugin;
import org.dromara.soul.web.plugin.function.RewritePlugin;
import org.dromara.soul.web.plugin.function.WebSocketPlugin;
import org.dromara.soul.web.plugin.ratelimter.RedisRateLimiter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.SearchStrategy;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.web.reactive.socket.client.ReactorNettyWebSocketClient;
import org.springframework.web.reactive.socket.client.WebSocketClient;
import org.springframework.web.reactive.socket.server.WebSocketService;
import org.springframework.web.reactive.socket.server.support.HandshakeWebSocketService;
import org.springframework.web.server.WebFilter;

import java.util.List;
import java.util.stream.Collectors;

/**
 * SoulConfiguration.
 *
 * @author xiaoyu(Myth)
 */
@Configuration
@ComponentScan("org.dromara.soul")
public class SoulConfiguration {

    private final ZookeeperCacheManager zookeeperCacheManager;

    private final SoulEventPublisher soulEventPublisher;

    private final RedisRateLimiter redisRateLimiter;

    private final UpstreamCacheManager upstreamCacheManager;

    /**
     * Instantiates a new Soul configuration.
     *
     * @param zookeeperCacheManager the zookeeper cache manager
     * @param soulEventPublisher    the soul event publisher
     * @param redisRateLimiter      the redis rate limiter
     * @param upstreamCacheManager  the upstream cache manager
     */
    @Autowired(required = false)
    public SoulConfiguration(final ZookeeperCacheManager zookeeperCacheManager,
                             final SoulEventPublisher soulEventPublisher,
                             final RedisRateLimiter redisRateLimiter,
                             final UpstreamCacheManager upstreamCacheManager) {
        this.zookeeperCacheManager = zookeeperCacheManager;
        this.soulEventPublisher = soulEventPublisher;
        this.redisRateLimiter = redisRateLimiter;
        this.upstreamCacheManager = upstreamCacheManager;
    }

    /**
     * init global plugin.
     *
     * @return {@linkplain GlobalPlugin}
     */
    @Bean
    public SoulPlugin globalPlugin() {
        return new GlobalPlugin();
    }


    /**
     * init sign plugin.
     *
     * @return {@linkplain SignPlugin}
     */
    @Bean
    public SoulPlugin signPlugin() {
        return new SignPlugin(zookeeperCacheManager);
    }

    /**
     * init waf plugin.
     *
     * @return {@linkplain WafPlugin}
     */
    @Bean
    public SoulPlugin wafPlugin() {
        return new WafPlugin(zookeeperCacheManager);
    }

    /**
     * init monitor plugin.
     *
     * @return {@linkplain MonitorPlugin}
     */
    @Bean
    public SoulPlugin monitorPlugin() {
        return new MonitorPlugin(soulEventPublisher, zookeeperCacheManager);
    }

    /**
     * init rateLimiterPlugin.
     *
     * @return {@linkplain RateLimiterPlugin}
     */
    @Bean
    public SoulPlugin rateLimiterPlugin() {
        return new RateLimiterPlugin(zookeeperCacheManager, redisRateLimiter);
    }

    /**
     * init rewritePlugin.
     *
     * @return {@linkplain RewritePlugin}
     */
    @Bean
    public SoulPlugin rewritePlugin() {
        return new RewritePlugin(zookeeperCacheManager);
    }

    /**
     * init dividePlugin.
     *
     * @return {@linkplain DividePlugin}
     */
    @Bean
    public SoulPlugin dividePlugin() {
        return new DividePlugin(zookeeperCacheManager, upstreamCacheManager);
    }

    /**
     * Web socket plugin web socket plugin.
     *
     * @param webSocketClient  the web socket client
     * @param webSocketService the web socket service
     * @return the web socket plugin
     */
    @Bean
    public WebSocketPlugin webSocketPlugin(final WebSocketClient webSocketClient,
                                           final WebSocketService webSocketService) {
        return new WebSocketPlugin(zookeeperCacheManager, upstreamCacheManager, webSocketClient, webSocketService);
    }

    /**
     * init responsePlugin.
     *
     * @return {@linkplain ResponsePlugin}
     */
    @Bean
    public SoulPlugin responsePlugin() {
        return new ResponsePlugin();
    }

    /**
     * init SoulWebHandler.
     *
     * @param plugins this plugins is All impl SoulPlugin.
     * @return {@linkplain SoulWebHandler}
     */
    @Bean
    public SoulWebHandler soulWebHandler(final List<SoulPlugin> plugins) {
        final List<SoulPlugin> soulPlugins = plugins.stream()
                .sorted((m, n) -> {
                    if (m.pluginType().equals(n.pluginType())) {
                        return m.getOrder() - n.getOrder();
                    } else {
                        return m.pluginType().getName().compareTo(n.pluginType().getName());
                    }
                }).collect(Collectors.toList());
        return new SoulWebHandler(soulPlugins);
    }

    /**
     * init  SoulHandlerMapping.
     *
     * @param soulWebHandler {@linkplain SoulWebHandler}
     * @return {@linkplain SoulHandlerMapping}
     */
    @Bean
    public SoulHandlerMapping soulHandlerMapping(final SoulWebHandler soulWebHandler) {
        return new SoulHandlerMapping(soulWebHandler);
    }

    /**
     * Body web filter web filter.
     *
     * @return the web filter
     */
    @Bean
    @Order(-1)
    public WebFilter bodyWebFilter() {
        return new BodyWebFilter();
    }

    /**
     * init param web filter.
     *
     * @return {@linkplain ParamWebFilter}
     */
    @Bean
    @Order(1)
    public WebFilter paramWebFilter() {
        return new ParamWebFilter();
    }

    /**
     * init time web filter.
     *
     * @return {@linkplain TimeWebFilter}
     */
    @Bean
    @Order(2)
    @ConditionalOnProperty(name = "soul.timeVerify.enabled", matchIfMissing = true)
    public WebFilter timeWebFilter() {
        return new TimeWebFilter();
    }


    /**
     * Web socket web filter web filter.
     *
     * @return the web filter
     */
    @Bean
    @Order(2)
    public WebFilter webSocketWebFilter() {
        return new WebSocketWebFilter();
    }

    /**
     * Generic param service generic param service.
     *
     * @return the generic param service
     */
    @Bean
    @ConditionalOnMissingBean(value = GenericParamService.class, search = SearchStrategy.ALL)
    public GenericParamService genericParamService() {
        return new GenericParamServiceImpl();
    }


    /**
     * Reactor netty web socket client reactor netty web socket client.
     *
     * @return the reactor netty web socket client
     */
    @Bean
    public ReactorNettyWebSocketClient reactorNettyWebSocketClient() {
        return new ReactorNettyWebSocketClient();
    }

    /**
     * Web socket service web socket service.
     *
     * @return the web socket service
     */
    @Bean
    public WebSocketService webSocketService() {
        return new HandshakeWebSocketService();
    }


}
