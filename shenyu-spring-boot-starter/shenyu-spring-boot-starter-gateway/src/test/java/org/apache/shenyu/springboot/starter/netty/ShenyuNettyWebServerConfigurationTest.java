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
                "shenyu.netty.http.accessLog=false",
                "shenyu.netty.http.web-server-factory-enabled=false",
                "shenyu.netty.http.selectCount=10",
                "shenyu.netty.http.workerCount=2",
                "shenyu.netty.http.serverSocketChannel.soBacklog=64",
                "shenyu.netty.http.serverSocketChannel.connectTimeoutMillis=65536",
                "shenyu.netty.http.serverSocketChannel.writeBufferLowWaterMark=65536",
                "shenyu.netty.http.serverSocketChannel.soRcvBuf=65536",
                "shenyu.netty.http.socketChannel.soSndBuf=128",
                "shenyu.netty.http.socketChannel.ipTos=64",
                "shenyu.netty.http.socketChannel.soKeepAlive=true",
                "shenyu.netty.http.socketChannel.allowHalfClosure=true",
                "shenyu.netty.http.socketChannel.soLinger=10",
                "shenyu.netty.http.socketChannel.tcpNoDelay=false",
                "shenyu.netty.http.socketChannel.writeBufferHighWaterMark=256",
                "shenyu.netty.http.socketChannel.writeBufferLowWaterMark=128",
                "shenyu.netty.http.socketChannel.writeSpinCount=8",
                "shenyu.netty.http.socketChannel.autoRead=false",
                "shenyu.netty.http.socketChannel.soReuseAddr=true",
                "shenyu.netty.http.socketChannel.soRcvBuf=65536",
                "shenyu.netty.http.socketChannel.messageSizeEstimator=8",
                "shenyu.netty.http.socketChannel.singleEventExecutorPerGroup=false"
            )
            .run(context -> {
                NettyHttpProperties properties = context.getBean("nettyTcpProperties", NettyHttpProperties.class);
                assertNotNull(properties);
                assertThat(properties.getAccessLog(), is(false));
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
                assertThat(properties.getSocketChannel().getMessageSizeEstimator(), is(8));
                assertThat(properties.getSocketChannel().getSingleEventExecutorPerGroup(), is(false));
            });
    }
}
