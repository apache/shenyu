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

package org.apache.shenyu.protocol.mqtt;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.ChannelFuture;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.util.ResourceLeakDetector;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.BaseRepository;
import org.reflections.Reflections;

import java.util.Locale;

/**
 * mqtt server.
 */
public class MqttBootstrapServer implements BootstrapServer {

    private static final String REPOSITORY_PACKAGE_NAME = "org.apache.shenyu.protocol.mqtt.repositories";

    private static final MqttContext ENV = new MqttContext();

    private EventLoopGroup bossGroup;

    private EventLoopGroup workerGroup;

    private ChannelFuture future;

    @Override
    public void init() {
        try {
            initRepositories();
        } catch (Exception e) {
            //// todo log
        }
    }

    @Override
    public void start() {
        //// todo thread start mqtt server
        ResourceLeakDetector.setLevel(ResourceLeakDetector.Level.valueOf(ENV.getLeakDetectorLevel().toUpperCase(Locale.ROOT)));
        bossGroup = new NioEventLoopGroup(ENV.getBossGroupThreadCount());
        workerGroup = new NioEventLoopGroup(ENV.getWorkerGroupThreadCount());
        ServerBootstrap bootstrap = new ServerBootstrap();
        bootstrap.group(bossGroup, workerGroup)
                .channel(NioServerSocketChannel.class)
                .childHandler(new MqttTransportServerInitializer(ENV.getMaxPayloadSize()));
        try {
            future = bootstrap.bind(ENV.getPort()).sync();
            //// todo log
        } catch (InterruptedException e) {
            //// todo log
        }
    }

    @Override
    public void shutdown() {
        bossGroup.shutdownGracefully();
        workerGroup.shutdownGracefully();
        future.channel().close();
    }

    private void initRepositories() throws IllegalAccessException, InstantiationException {
        Reflections reflections = new Reflections(REPOSITORY_PACKAGE_NAME);
        for (Class<? extends BaseRepository> clazz : reflections.getSubTypesOf(BaseRepository.class)) {
            Singleton.INST.single(clazz, clazz.newInstance());
        }
    }

}
