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

package org.dromara.soul.web.configuration;

import org.dromara.soul.web.cache.LocalCacheManager;
import org.dromara.soul.web.cache.UpstreamCacheManager;
import org.dromara.soul.web.config.HttpClientProperties;
import org.dromara.soul.web.config.SoulConfig;
import org.dromara.soul.web.disruptor.publisher.SoulEventPublisher;
import org.dromara.soul.web.filter.DefaultParamService;
import org.dromara.soul.web.filter.FileSizeFilter;
import org.dromara.soul.web.filter.ParamService;
import org.dromara.soul.web.filter.ParamWebFilter;
import org.dromara.soul.web.filter.TimeWebFilter;
import org.dromara.soul.web.filter.WebSocketWebFilter;
import org.dromara.soul.web.handler.SoulWebHandler;
import org.dromara.soul.web.influxdb.service.InfluxDbService;
import org.dromara.soul.web.plugin.SoulPlugin;
import org.dromara.soul.web.plugin.after.MonitorPlugin;
import org.dromara.soul.web.plugin.before.DefaultSignService;
import org.dromara.soul.web.plugin.before.GlobalPlugin;
import org.dromara.soul.web.plugin.before.SignPlugin;
import org.dromara.soul.web.plugin.before.SignService;
import org.dromara.soul.web.plugin.before.WafPlugin;
import org.dromara.soul.web.plugin.function.DividePlugin;
import org.dromara.soul.web.plugin.function.RateLimiterPlugin;
import org.dromara.soul.web.plugin.function.RewritePlugin;
import org.dromara.soul.web.plugin.function.WebSocketPlugin;
import org.dromara.soul.web.plugin.ratelimter.RedisRateLimiter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
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
@Import(value = {DubboConfiguration.class, LocalCacheConfiguration.class, ErrorHandlerConfiguration.class,
        SoulExtConfiguration.class, HttpClientConfiguration.class, SpringExtConfiguration.class})
@EnableConfigurationProperties({HttpClientProperties.class})
public class SoulConfiguration {

    private final LocalCacheManager localCacheManager;

    private final UpstreamCacheManager upstreamCacheManager;

    /**
     * Instantiates a new Soul configuration.
     *
     * @param localCacheManager    the local cache manager
     * @param upstreamCacheManager the upstream cache manager
     */
    @Autowired(required = false)
    public SoulConfiguration(@Qualifier("localCacheManager") final LocalCacheManager localCacheManager,
                             final UpstreamCacheManager upstreamCacheManager) {
        this.localCacheManager = localCacheManager;
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
     * @param signService the sign service
     * @return {@linkplain SignPlugin}
     */
    @Bean
    public SoulPlugin signPlugin(final SignService signService) {
        return new SignPlugin(signService);
    }

    /**
     * Sign service sign service.
     *
     * @return the sign service
     */
    @Bean
    @ConditionalOnMissingBean(SignService.class)
    public SignService signService() {
        return new DefaultSignService(localCacheManager);
    }

    /**
     * init waf plugin.
     *
     * @return {@linkplain WafPlugin}
     */
    @Bean
    public SoulPlugin wafPlugin() {
        return new WafPlugin(localCacheManager);
    }


    /**
     * init rateLimiterPlugin.
     *
     * @return {@linkplain RateLimiterPlugin}
     */
    @Bean
    public SoulPlugin rateLimiterPlugin() {
        return new RateLimiterPlugin(localCacheManager, redisRateLimiter());
    }


    /**
     * Redis rate limiter redis rate limiter.
     *
     * @return the redis rate limiter
     */
    @Bean
    @ConditionalOnMissingBean
    public RedisRateLimiter redisRateLimiter() {
        return new RedisRateLimiter();
    }

    /**
     * init rewritePlugin.
     *
     * @return {@linkplain RewritePlugin}
     */
    @Bean
    public SoulPlugin rewritePlugin() {
        return new RewritePlugin(localCacheManager);
    }

    /**
     * init dividePlugin.
     *
     * @return {@linkplain DividePlugin}
     */
    @Bean
    public SoulPlugin dividePlugin() {
        return new DividePlugin(localCacheManager, upstreamCacheManager);
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
        return new WebSocketPlugin(localCacheManager, upstreamCacheManager, webSocketClient, webSocketService);
    }

    /**
     * Influx db service influx db service.
     *
     * @return the influx db service
     */
    @Bean
    public InfluxDbService influxDbService() {
        return new InfluxDbService();
    }

    /**
     * Soul event publisher soul event publisher.
     *
     * @param influxDbService the influx db service
     * @return the soul event publisher
     */
    @Bean
    public SoulEventPublisher soulEventPublisher(final InfluxDbService influxDbService) {
        return new SoulEventPublisher(influxDbService);
    }

    /**
     * Monitor plugin soul plugin.
     *
     * @param soulEventPublisher the soul event publisher
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin monitorPlugin(final SoulEventPublisher soulEventPublisher) {
        return new MonitorPlugin(soulEventPublisher, localCacheManager);
    }

    /**
     * init SoulWebHandler.
     *
     * @param plugins this plugins is All impl SoulPlugin.
     * @return {@linkplain SoulWebHandler}
     */
    @Bean("webHandler")
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
     * Body web filter web filter.
     *
     * @return the web filter
     */
    @Bean
    @Order(-10)
    public WebFilter bodySizeFilter() {
        return new FileSizeFilter();
    }

    /**
     * Param web filter web filter.
     *
     * @param paramService the param service
     * @return the web filter
     */
    @Bean
    @Order(1)
    public WebFilter paramWebFilter(final ParamService paramService) {
        return new ParamWebFilter(paramService);
    }


    /**
     * init time web filter.
     *
     * @param soulConfig the soul config
     * @return {@linkplain TimeWebFilter}
     */
    @Bean
    @Order(30)
    @ConditionalOnProperty(name = "soul.filterTimeEnable")
    public WebFilter timeWebFilter(final SoulConfig soulConfig) {
        return new TimeWebFilter(soulConfig);
    }

    /**
     * Web socket web filter web filter.
     *
     * @return the web filter
     */
    @Bean
    @Order(4)
    public WebFilter webSocketWebFilter() {
        return new WebSocketWebFilter();
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
