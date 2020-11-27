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

package org.dromara.soul.plugin.monitor;

import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.metrics.enums.MetricsLabelEnum;
import org.dromara.soul.metrics.facade.MetricsTrackerFacade;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.base.AbstractSoulPlugin;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * the monitor plugin.
 *
 * @author xiaoyu(Myth)
 */
public class MonitorPlugin extends AbstractSoulPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        MetricsTrackerFacade.getInstance().counterInc(MetricsLabelEnum.HTTP_REQUEST_TOTAL.getName(), exchange.getRequest().getURI().getPath(), exchange.getRequest().getMethodValue());
        return chain.execute(exchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.MONITOR.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.MONITOR.getName();
    }

}
