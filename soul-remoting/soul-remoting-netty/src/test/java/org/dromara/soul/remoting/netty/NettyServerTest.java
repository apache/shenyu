/*
 *
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.remoting.netty;

import org.dromara.soul.common.Attribute;
import org.dromara.soul.common.extension.ExtensionLoader;
import org.dromara.soul.common.http.HttpSoulResponse;
import org.dromara.soul.common.http.HttpStatus;
import org.dromara.soul.remoting.api.Channel;
import org.dromara.soul.remoting.api.ChannelHandler;
import org.dromara.soul.remoting.api.NetServer;
import org.dromara.soul.remoting.api.ServerTransport;
import org.junit.Test;

/**
 * NettyServerTest
 * CreateDate: 2019/10/14 15:32
 *
 * @author sixh
 */
public class NettyServerTest {
    @Test
    public void testServer() throws InterruptedException {
        ExtensionLoader<ServerTransport> extensionLoader = ExtensionLoader.getExtensionLoader(ServerTransport.class);
        ServerTransport netty = extensionLoader.getJoin("netty");
        Attribute attribute = new Attribute();
        NetServer bind = netty.bind(attribute, new ChannelHandler() {
            @Override
            public void connected(Channel channel) {
                System.out.println(channel.remoteAddress());
            }

            @Override
            public void disconnected(Channel channel) {

            }

            @Override
            public void sent(Channel channel, Object message) {
                System.out.println(message);
            }

            @Override
            public void received(Channel channel, Object message) {
                System.out.println(message);
                HttpSoulResponse response = new HttpSoulResponse();
                response.setStatus(HttpStatus.OK.getCode());
                response.setBody("{name:123}");
                channel.send(response);
            }

            @Override
            public void exceptionCaught(Channel channel, Throwable cause) {

            }

            @Override
            public void timeout(Channel channel) {
                System.out.println(channel.remoteAddress() + "通道超时......");
            }
        });
        bind.bind();
        Thread.sleep(Integer.MAX_VALUE);
    }
}
