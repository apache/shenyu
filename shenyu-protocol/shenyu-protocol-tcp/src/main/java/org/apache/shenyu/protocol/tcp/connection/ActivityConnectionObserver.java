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

package org.apache.shenyu.protocol.tcp.connection;

import com.google.common.eventbus.Subscribe;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.netty.Connection;
import reactor.netty.ConnectionObserver;

import java.net.SocketAddress;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * ActivityConnectionObserver.
 */
public class ActivityConnectionObserver implements ConnectionObserver {
    private static final Logger LOG = LoggerFactory.getLogger(ActivityConnectionObserver.class);

    private final Map<Connection, State> cache = new ConcurrentHashMap<>();

    private final String name;

    public ActivityConnectionObserver(final String name) {
        this.name = name;
    }

    @Override
    public void onStateChange(final Connection connection, final State newState) {
        if (newState == State.CONNECTED) {
            cache.put(connection, newState);
            LOG.info("{} add connection into cache ={}", name, connection);
        } else if (newState == State.DISCONNECTING
                || newState == State.RELEASED
        ) {
            cache.remove(connection);
            LOG.info("{} remove connection into cache ={}", name, connection);
        } else {
            if (cache.containsKey(connection)) {
                cache.put(connection, newState);
            }
        }
    }

    /**
     * onRemove.
     *
     * @param remove removeList
     */
    @Subscribe
    public void onRemove(final List<DiscoveryUpstreamData> remove) {
        LOG.info("shenyu {} ConnectionObserver  do on remove upstreams", name);
        for (Connection connection : cache.keySet()) {
            SocketAddress socketAddress = connection.channel().remoteAddress();
            if (in(remove, socketAddress)) {
                LOG.info("shenyu dispose {} connection ", connection);
                connection.disposeNow();
            }
        }
    }

    /**
     * in.
     *
     * @param removeList         removeList
     * @param cacheSocketAddress cacheSocketAddress
     * @return boolean
     */
    private boolean in(final List<DiscoveryUpstreamData> removeList, final SocketAddress cacheSocketAddress) {
        return removeList.stream().anyMatch(u -> {
            String cacheUrl = cacheSocketAddress.toString().substring(1);
            String removedUrl = u.getUrl();
            LOG.info("compare {} , {}", cacheUrl, removedUrl);
            return StringUtils.equals(cacheUrl, removedUrl);
        });
    }
}
