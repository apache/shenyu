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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.protocol.tcp.UpstreamProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;


/**
 * ClientConnectionConfigProviderFactory.
 */
public class DefaultConnectionConfigProvider implements ClientConnectionConfigProvider {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultConnectionConfigProvider.class);

    private final String loadBalanceAlgorithm;

    private final String pluginSelectorName;

    public DefaultConnectionConfigProvider(final String loadBalanceAlgorithm, final String pluginSelectorName) {
        this.loadBalanceAlgorithm = loadBalanceAlgorithm;
        this.pluginSelectorName = pluginSelectorName;
    }

    @Override
    public URI getProxiedService(final String ip) {
        List<Upstream> upstreamList = UpstreamProvider.getSingleton().provide(this.pluginSelectorName).stream().map(dp -> Upstream.builder()
                .url(dp.getUrl())
                .status(open(dp.getStatus()))
                .weight(dp.getWeight())
                .protocol(dp.getProtocol())
                .warmup(JsonUtils.jsonToMap(dp.getProps(), Integer.class).get("warmupTime"))
                .timestamp(dp.getDateCreated().getTime())
                .build()).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(upstreamList)) {
            throw new ShenyuException("shenyu TcpProxy don't have any upstream");
        }
        Upstream upstream = LoadBalancerFactory.selector(upstreamList, loadBalanceAlgorithm, ip);
        return cover(upstream);
    }

    private URI cover(final Upstream upstream) {
        try {
            return new URI(upstream.getProtocol() + "://" + upstream.getUrl());
        } catch (URISyntaxException e) {
            LOG.error("Upstream url is wrong", e);
            throw new ShenyuException(e);
        }
    }

    /**
     * false close, true open.
     * @param status status  (0, healthy, 1 unhealthy)
     * @return openStatus false close, true open.
     */
    private boolean open(final int status) {
        return Objects.equals(status, 0);
    }

}
