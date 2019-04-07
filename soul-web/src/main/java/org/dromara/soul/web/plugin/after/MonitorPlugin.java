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

package org.dromara.soul.web.plugin.after;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.zk.RuleZkDTO;
import org.dromara.soul.common.dto.zk.SelectorZkDTO;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.web.cache.ZookeeperCacheManager;
import org.dromara.soul.web.disruptor.publisher.SoulEventPublisher;
import org.dromara.soul.web.influxdb.entity.MonitorDO;
import org.dromara.soul.web.plugin.AbstractSoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * the monitor plugin.
 *
 * @author xiaoyu(Myth)
 */
public class MonitorPlugin extends AbstractSoulPlugin {

    private final SoulEventPublisher soulEventPublisher;

    /**
     * Instantiates a new Monitor plugin.
     *
     * @param soulEventPublisher    the soul event publisher
     * @param zookeeperCacheManager the zookeeper cache manager
     */
    public MonitorPlugin(final SoulEventPublisher soulEventPublisher,
                         final ZookeeperCacheManager zookeeperCacheManager) {
        super(zookeeperCacheManager);
        this.soulEventPublisher = soulEventPublisher;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorZkDTO selector, final RuleZkDTO rule) {
        soulEventPublisher.publishEvent(buildMonitorData(exchange));
        return chain.execute(exchange);
    }

    @Override
    public String named() {
        return PluginEnum.MONITOR.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.MONITOR.getCode();
    }

    private MonitorDO buildMonitorData(final ServerWebExchange exchange) {
        final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);
        MonitorDO visitorDO = new MonitorDO();
        String result = exchange.getAttribute(Constants.CLIENT_RESPONSE_RESULT_TYPE);
        if (StringUtils.isBlank(result)) {
            visitorDO.setResultType(ResultEnum.ERROR.getName());
        } else {
            visitorDO.setResultType(result);
        }
        visitorDO.setRpcType(Objects.requireNonNull(requestDTO).getRpcType());
        visitorDO.setCount(1);
        visitorDO.setModule(requestDTO.getModule());
        visitorDO.setMethod(requestDTO.getMethod());
        visitorDO.setIp(Objects.requireNonNull(exchange.getRequest().getRemoteAddress()).getAddress().getHostAddress());
        visitorDO.setHost(exchange.getRequest().getRemoteAddress().getHostString());
        return visitorDO;
    }

    /**
     * return plugin type.
     *
     * @return {@linkplain PluginTypeEnum}
     */
    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.LAST;
    }
}
