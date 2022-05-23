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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link ShenyuNettyWebServerConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class ShenyuNettyWebServerConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(ShenyuNettyWebServerConfiguration.class))
                .withBean(ShenyuNettyWebServerConfigurationTest.class);
    }

    @Test
    public void testNettyProperties() {
        applicationContextRunner
            .withPropertyValues(
                "debug=true",
                "shenyu.netty.tcp.webServerFactoryEnabled=false",
                "shenyu.netty.tcp.selectCount=10",
                "shenyu.netty.tcp.workerCount=2",
                "shenyu.netty.tcp.serverSocketChannel.soBacklog=64",
                "shenyu.netty.tcp.serverSocketChannel.connectTimeoutMillis=65536",
                "shenyu.netty.tcp.serverSocketChannel.writeBufferLowWaterMark=65536",
                "shenyu.netty.tcp.serverSocketChannel.soRcvBuf=65536",
                "shenyu.netty.tcp.socketChannel.soSndBuf=128",
                "shenyu.netty.tcp.socketChannel.ipTos=64",
                "shenyu.netty.tcp.socketChannel.soKeepAlive=true",
                "shenyu.netty.tcp.socketChannel.allowHalfClosure=true",
                "shenyu.netty.tcp.socketChannel.soLinger=10",
                "shenyu.netty.tcp.socketChannel.tcpNoDelay=false",
                "shenyu.netty.tcp.socketChannel.writeBufferHighWaterMark=256",
                "shenyu.netty.tcp.socketChannel.writeBufferLowWaterMark=128",
                "shenyu.netty.tcp.socketChannel.writeSpinCount=8",
                "shenyu.netty.tcp.socketChannel.autoRead=false",
                "shenyu.netty.tcp.socketChannel.soReuseAddr=true",
                "shenyu.netty.tcp.socketChannel.soRcvBuf=65536"
            )
            .run(context -> {
                NettyTcpProperties properties = context.getBean("nettyTcpProperties", NettyTcpProperties.class);
                assertNotNull(properties);
                assertThat(properties.isWebServerFactoryEnabled(), is(false));
                assertThat(properties.getSelectCount(), is(10));
                assertThat(properties.getWorkerCount(), is(2));
                assertNotNull(properties.getServerSocketChannel());
                assertThat(properties.getServerSocketChannel().getSoBacklog(), is(64));
                assertThat(properties.getServerSocketChannel().getConnectTimeoutMillis(), is(65536));
                assertThat(properties.getServerSocketChannel().getWriteBufferLowWaterMark(), is(65536));
                assertThat(properties.getServerSocketChannel().getSoRcvBuf(), is(65536));
                assertNotNull(properties.getSocketChannel());
                assertThat(properties.getSocketChannel().getSoSndBuf(), is(128));
                assertThat(properties.getSocketChannel().getIpTos(), is(64));
                assertThat(properties.getSocketChannel().isSoKeepAlive(), is(true));
                assertThat(properties.getSocketChannel().isAllowHalfClosure(), is(true));
                assertThat(properties.getSocketChannel().getSoLinger(), is(10));
                assertThat(properties.getSocketChannel().isTcpNoDelay(), is(false));
                assertThat(properties.getSocketChannel().getWriteBufferHighWaterMark(), is(256));
                assertThat(properties.getSocketChannel().getWriteBufferLowWaterMark(), is(128));
                assertThat(properties.getSocketChannel().getWriteSpinCount(), is(8));
                assertThat(properties.getSocketChannel().isAutoRead(), is(false));
                assertThat(properties.getSocketChannel().isSoReuseAddr(), is(true));
                assertThat(properties.getSocketChannel().getSoRcvBuf(), is(65536));
            });
    }
}
