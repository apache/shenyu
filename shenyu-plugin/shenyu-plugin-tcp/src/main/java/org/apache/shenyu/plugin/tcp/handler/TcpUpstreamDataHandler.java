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

package org.apache.shenyu.plugin.tcp.handler;

import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.protocol.tcp.BootstrapServer;
import org.apache.shenyu.protocol.tcp.TcpServerConfiguration;
import org.apache.shenyu.protocol.tcp.UpstreamProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * upstreamList data change
 */
public class TcpUpstreamDataHandler {
    private static final Logger LOG = LoggerFactory.getLogger(TcpUpstreamDataHandler.class);

    private final Map<String, BootstrapServer> cache = new ConcurrentHashMap<>();

    /**
     * handle.
     *
     * @param proxySelectorData proxySelectorData
     * @param action            action
     * @param upstreamList      upstreamList
     */
    public void handle(final ProxySelectorData proxySelectorData, final Action action, final List<Upstream> upstreamList) {
        String name = proxySelectorData.getName();
        BootstrapServer bootstrapServer;
        switch (action) {
            case CREATE:
                Integer forwardPort = proxySelectorData.getForwardPort();
                TcpServerConfiguration tcpServerConfiguration = new TcpServerConfiguration();
                tcpServerConfiguration.setPort(forwardPort);
                tcpServerConfiguration.setProps(proxySelectorData.getProps());
                tcpServerConfiguration.setPluginSelectorName(name);
                UpstreamProvider.getSingleton().createUpstreams(name, upstreamList);
                bootstrapServer = TcpBootstrapFactory.getSingleton().createBootstrapServer(tcpServerConfiguration);
                cache.put(name, bootstrapServer);
                LOG.info("shenyu create TcpBootstrapServer success port is {}", forwardPort);
                break;
            case UPDATE:
                List<Upstream> removed = UpstreamProvider.getSingleton().refreshCache(name, upstreamList);
                bootstrapServer = cache.get(name);
                bootstrapServer.doOnUpdate(removed);
                LOG.info("shenyu update TcpBootstrapServer success remove is {}", removed);
                break;
            case DELETE:
                cache.remove(name).shutdown();
                break;
        }

    }

    enum Action {
        CREATE,
        UPDATE,
        DELETE
    }

    // 测试 自动切换
    public static void main(String[] args) throws IOException, InterruptedException {
        ProxySelectorData proxySelectorData = new ProxySelectorData();
        proxySelectorData.setId("1");
        proxySelectorData.setForwardPort(9500);
        proxySelectorData.setName("demo-selector");
        TcpUpstreamDataHandler tcpUpstreamDataHandler = new TcpUpstreamDataHandler();
        Upstream build = Upstream.builder().protocol("tcp").url("127.0.0.1:9095").build();
        tcpUpstreamDataHandler.handle(proxySelectorData, Action.CREATE, Arrays.asList(build));
        Thread.sleep(20000);
        System.out.println("change");
        tcpUpstreamDataHandler.handle(proxySelectorData, Action.UPDATE, Arrays.asList(Upstream.builder().protocol("tcp").url("127.0.0.1:9096").build()));
        System.out.println("changed");
        System.in.read();
    }

}
