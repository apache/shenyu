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

package org.dromara.soul.plugin.divide.balance.spi;

import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.plugin.divide.balance.LoadBalance;

import java.util.List;

/**
 * The type Abstract load balance.
 *
 * @author xiaoyu(Myth)
 */
public abstract class AbstractLoadBalance implements LoadBalance {

    /**
     * Do select divide upstream.
     *
     * @param upstreamList the upstream list
     * @param ip           the ip
     * @return the divide upstream
     */
    protected abstract DivideUpstream doSelect(List<DivideUpstream> upstreamList, String ip);

    @Override
    public DivideUpstream select(final List<DivideUpstream> upstreamList, final String ip) {
        if (CollectionUtils.isEmpty(upstreamList)) {
            return null;
        }
        if (upstreamList.size() == 1) {
            return upstreamList.get(0);
        }
        return doSelect(upstreamList, ip);
    }

    protected int getWeight(final DivideUpstream upstream) {

        int weight = getWeight(upstream.getTimestamp(), 60000, upstream.getWeight());
        return weight;
    }

    private int getWeight(final long timestamp,final int warmup,final int urlWeight) {

        if (urlWeight > 0 && timestamp > 0) {
            int uptime = (int) (System.currentTimeMillis() - timestamp);

            if (uptime > 0 && uptime < warmup) {
                return calculateWarmupWeight(uptime, warmup, urlWeight);

            }
        }
        return urlWeight;
    }

    private int calculateWarmupWeight(final int uptime,final int warmup,final int urlWeight) {
        int ww = (int) ((float) uptime / ((float) warmup / (float) urlWeight));
        return ww < 1 ? 1 : (ww > urlWeight ? urlWeight : ww);
    }

}
