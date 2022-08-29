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

package org.apache.shenyu.loadbalancer.spi;

import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.entity.UpstreamHolder;
import org.apache.shenyu.loadbalancer.util.WeightUtil;

/**
 * The type Abstract load balancer.
 */
public abstract class AbstractLoadBalancer implements LoadBalancer {

    /**
     * Do select upstream.
     * Deprecated
     *
     * @deprecated {@link AbstractLoadBalancer#doSelect(UpstreamHolder, String)}
     * @param upstreamList  the upstreams
     * @param ip            the ip
     * @return the upstream
     */
    @Deprecated
    protected abstract Upstream doSelect(List<Upstream> upstreamList, String ip);

    /**
     * Do select upstream.
     *
     * @param upstreamHolder wrapper of upstream
     * @param ip             the ip
     * @return the upstream
     */
    protected abstract Upstream doSelect(UpstreamHolder upstreamHolder, String ip);

    @Override
    public Upstream select(final List<Upstream> upstreamList, final String ip) {
        return select(new UpstreamHolder(WeightUtil.calculateTotalWeight(upstreamList), upstreamList), ip);
    }

    @Override
    public Upstream select(final UpstreamHolder upstreamHolder, final String ip) {
        List<Upstream> upstreamList = upstreamHolder.getUpstreams();
        if (CollectionUtils.isEmpty(upstreamList)) {
            return null;
        }
        if (upstreamList.size() == 1) {
            return upstreamList.get(0);
        }
        return doSelect(upstreamHolder, ip);
    }

    /**
     * get weight.
     * Deprecated
     *
     * @deprecated {@link WeightUtil#getWeight(Upstream)}
     * @param upstream upstream
     * @return weight
     */
    @Deprecated
    protected int getWeight(final Upstream upstream) {
        if (!upstream.isStatus()) {
            return 0;
        }
        return WeightUtil.getWeight(upstream);
    }

}
