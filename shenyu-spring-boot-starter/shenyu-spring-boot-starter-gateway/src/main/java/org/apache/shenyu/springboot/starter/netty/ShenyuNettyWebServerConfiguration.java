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

package org.apache.shenyu.springboot.starter.netty;

import io.netty.channel.ChannelOption;
import io.netty.channel.WriteBufferWaterMark;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.config.NettyHttpProperties;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.config.ssl.ShenyuSniAsyncMapping;
import org.apache.shenyu.common.config.ssl.SslCrtAndKeyFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.web.embedded.netty.NettyReactiveWebServerFactory;
import org.springframework.boot.web.embedded.netty.NettyServerCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;
import reactor.netty.tcp.SslProvider;
import reactor.netty.tcp.TcpSslContextSpec;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

/**
 * The type shenyu netty web server factory.
 */
@Configuration
public class ShenyuNettyWebServerConfiguration {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuNettyWebServerConfiguration.class);
    
    /**
     * Netty tcp config.
     *
     * @return the netty tcp config
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.netty.http")
    public NettyHttpProperties nettyTcpProperties() {
        return new NettyHttpProperties();
    }

    /**
     * AsyncMapping for dynamic configure ssl.
     *
     * @return ShenyuSniAsyncMapping
     */
    @Bean
    @ConditionalOnProperty(value = {"shenyu.netty.http.web-server-factory-enabled", "shenyu.netty.http.sni.enabled"}, havingValue = "true")
    public ShenyuSniAsyncMapping shenyuSniAsyncMapping() {
        return new ShenyuSniAsyncMapping();
    }
    
    /**
     * Netty reactive web server factory.
     *
     * @param properties the properties
     * @param shenyuSniAsyncMappingProvider shenyuSniAsyncMapping
     * @param tcpSslContextSpecs default tcpSslContextSpecs
     * @return the netty reactive web server factory
     */
    @Bean
    @ConditionalOnProperty(value = "shenyu.netty.http.web-server-factory-enabled", havingValue = "true", matchIfMissing = true)
    public NettyReactiveWebServerFactory nettyReactiveWebServerFactory(final ObjectProvider<NettyHttpProperties> properties,
                                                                       final ObjectProvider<ShenyuSniAsyncMapping> shenyuSniAsyncMappingProvider,
                                                                       final ObjectProvider<TcpSslContextSpec> tcpSslContextSpecs) {
        NettyReactiveWebServerFactory webServerFactory = new NettyReactiveWebServerFactory();
        NettyHttpProperties nettyHttpProperties = Optional.ofNullable(properties.getIfAvailable()).orElse(new NettyHttpProperties());
        webServerFactory.addServerCustomizers(new EventLoopNettyCustomizer(nettyHttpProperties, httpServer -> {
            HttpServer server = httpServer;
            // Configure sni certificates
            NettyHttpProperties.SniProperties sniProperties = nettyHttpProperties.getSni();
            if (sniProperties.getEnabled()) {
                ShenyuSniAsyncMapping shenyuSniAsyncMapping = shenyuSniAsyncMappingProvider.getIfAvailable();
                if (Objects.isNull(shenyuSniAsyncMapping)) {
                    throw new ShenyuException("Can not find shenyuSniAsyncMapping bean");
                }
                if ("manual".equals(sniProperties.getMod())) {
                    List<SslCrtAndKeyFile> sslCrtAndKeyFiles = sniProperties.getCertificates();
                    if (CollectionUtils.isEmpty(sslCrtAndKeyFiles)) {
                        throw new ShenyuException("At least one certificate is required");
                    }

                    // Use the first certificate as the default certificate (this default certificate will not actually be used)
                    List<SslCrtAndKeyFile> certificates = sslCrtAndKeyFiles;
                    for (SslCrtAndKeyFile certificate : certificates) {
                        try {
                            shenyuSniAsyncMapping.addSslCertificate(certificate);
                        } catch (IOException e) {
                            LOG.error("add certificate error", e);
                        }
                    }

                    SslCrtAndKeyFile defaultCert = certificates.get(0);
                    TcpSslContextSpec defaultSpec = TcpSslContextSpec.forServer(new File(defaultCert.getKeyCertChainFile()),
                            new File(defaultCert.getKeyFile()));
                    
                    server = server.secure(spec -> spec.sslContext(defaultSpec)
                                .setSniAsyncMappings(shenyuSniAsyncMapping), false);
                } else if ("k8s".equals(sniProperties.getMod())) {
                    TcpSslContextSpec defaultSpec = Objects.requireNonNull(tcpSslContextSpecs.getIfAvailable());
                    server = server.secure(spec -> spec.sslContext(defaultSpec)
                            .setSniAsyncMappings(shenyuSniAsyncMapping), false);
                    shenyuSniAsyncMapping.addSslProvider("shenyu-default", SslProvider.builder().sslContext(defaultSpec).build());
                } else {
                    throw new ShenyuException("Cannot read the sni mod");
                }
            }
            return server;
        }));
        return webServerFactory;
    }

    private static class EventLoopNettyCustomizer implements NettyServerCustomizer {

        private final NettyHttpProperties nettyHttpProperties;

        private final Function<HttpServer, HttpServer> sniProcessor;
    
        /**
         * Instantiates a new Event loop netty customizer.
         *
         * @param nettyHttpProperties the netty tcp config
         */
        EventLoopNettyCustomizer(final NettyHttpProperties nettyHttpProperties, final Function<HttpServer, HttpServer> sniProcessor) {
            this.nettyHttpProperties = nettyHttpProperties;
            this.sniProcessor = sniProcessor;
        }

        @Override
        public HttpServer apply(final HttpServer httpServer) {
            return sniProcessor.apply(httpServer)
                    .runOn(LoopResources.create("shenyu-netty", nettyHttpProperties.getSelectCount(), nettyHttpProperties.getWorkerCount(), true))
                    .accessLog(nettyHttpProperties.getAccessLog())
                    // server socket channel parameters
                    .option(ChannelOption.SO_BACKLOG, nettyHttpProperties.getServerSocketChannel().getSoBacklog())
                    .option(ChannelOption.SO_REUSEADDR, nettyHttpProperties.getServerSocketChannel().isSoReuseAddr())
                    // common parameters
                    .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, nettyHttpProperties.getServerSocketChannel().getConnectTimeoutMillis())
                    .option(ChannelOption.WRITE_BUFFER_WATER_MARK, new WriteBufferWaterMark(nettyHttpProperties.getServerSocketChannel().getWriteBufferLowWaterMark(),
                            nettyHttpProperties.getServerSocketChannel().getWriteBufferHighWaterMark()))
                    .option(ChannelOption.WRITE_SPIN_COUNT, nettyHttpProperties.getServerSocketChannel().getWriteSpinCount())
                    .option(ChannelOption.AUTO_READ, nettyHttpProperties.getServerSocketChannel().isAutoRead())
                    .option(ChannelOption.ALLOCATOR, nettyHttpProperties.getServerSocketChannel().getAllocator())
                    .option(ChannelOption.MESSAGE_SIZE_ESTIMATOR, nettyHttpProperties.getServerSocketChannel().getMessageSizeEstimatorBuild())
                    .option(ChannelOption.SINGLE_EVENTEXECUTOR_PER_GROUP, nettyHttpProperties.getServerSocketChannel().getSingleEventExecutorPerGroup())

                    // socket channel parameters
                    .childOption(ChannelOption.SO_KEEPALIVE, nettyHttpProperties.getSocketChannel().isSoKeepAlive())
                    .childOption(ChannelOption.SO_LINGER, nettyHttpProperties.getSocketChannel().getSoLinger())
                    .childOption(ChannelOption.TCP_NODELAY, nettyHttpProperties.getSocketChannel().isTcpNoDelay())
                    .childOption(ChannelOption.IP_TOS, nettyHttpProperties.getSocketChannel().getIpTos())
                    .childOption(ChannelOption.ALLOW_HALF_CLOSURE, nettyHttpProperties.getSocketChannel().isAllowHalfClosure())
                    .childOption(ChannelOption.SO_REUSEADDR, nettyHttpProperties.getSocketChannel().isSoReuseAddr())
                    // common parameters
                    .childOption(ChannelOption.CONNECT_TIMEOUT_MILLIS, nettyHttpProperties.getSocketChannel().getConnectTimeoutMillis())
                    .childOption(ChannelOption.WRITE_BUFFER_WATER_MARK, new WriteBufferWaterMark(nettyHttpProperties.getSocketChannel().getWriteBufferLowWaterMark(),
                    nettyHttpProperties.getSocketChannel().getWriteBufferHighWaterMark()))
                    .childOption(ChannelOption.WRITE_SPIN_COUNT, nettyHttpProperties.getSocketChannel().getWriteSpinCount())
                    .childOption(ChannelOption.AUTO_READ, nettyHttpProperties.getSocketChannel().isAutoRead())
                    .childOption(ChannelOption.ALLOCATOR, nettyHttpProperties.getSocketChannel().getAllocator())
                    .childOption(ChannelOption.MESSAGE_SIZE_ESTIMATOR, nettyHttpProperties.getSocketChannel().getMessageSizeEstimatorBuild())
                    .childOption(ChannelOption.SINGLE_EVENTEXECUTOR_PER_GROUP, nettyHttpProperties.getSocketChannel().getSingleEventExecutorPerGroup());
        }
    }
}
