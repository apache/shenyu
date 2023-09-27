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

package org.apache.shenyu.plugin.base.utils;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.constant.LoadBalancerConstant;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * Change utils.
 */
public final class ChainUtils {

    /**
     * execute chain by load balancer.
     * @param exchange exchange
     * @param chain chain
     * @param upstream upstream
     * @param loadBalancer loadBalancer
     * @return Mono
     */
    public static Mono<Void> executeByLoadBalancer(final ServerWebExchange exchange, final ShenyuPluginChain chain, final Upstream upstream, final String loadBalancer) {
        if (upstream == null || StringUtils.isEmpty(loadBalancer)) {
            return chain.execute(exchange);
        }
        if (LoadBalancerConstant.P2C.equals(loadBalancer)) {
            return chain.execute(exchange).doOnSuccess(e -> responseTrigger(upstream
            )).doOnError(throwable -> responseTrigger(upstream));
        } else if (LoadBalancerConstant.SHORTEST_RESPONSE.equals(loadBalancer)) {
            final Long beginTime = System.currentTimeMillis();
            return chain.execute(exchange).doOnSuccess(e -> successResponseTrigger(upstream, beginTime
            ));
        }
        return chain.execute(exchange);
    }

    /**
     * p2c response trigger.
     * @param upstream upstream
     */
    private static void responseTrigger(final Upstream upstream) {
        long now = System.currentTimeMillis();
        upstream.getInflight().decrementAndGet();
        upstream.setResponseStamp(now);
        long stamp = upstream.getResponseStamp();
        long td = now - stamp;
        if (td < 0) {
            td = 0;
        }
        double w = Math.exp((double) -td / (double) 600);

        long lag = now - upstream.getLastPicked();
        if (lag < 0) {
            lag = 0;
        }
        long oldLag = upstream.getLag();
        if (oldLag == 0) {
            w = 0;
        }
        lag = (int) ((double) oldLag * w + (double) lag * (1.0 - w));
        upstream.setLag(lag);
    }

    /**
     * shortest response success response trigger.
     * @param upstream upstream
     */
    private static void successResponseTrigger(final Upstream upstream, final Long beginTime) {
        upstream.getSucceededElapsed().addAndGet(System.currentTimeMillis() - beginTime);
        upstream.getSucceeded().incrementAndGet();
    }
}
