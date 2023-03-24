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

import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.spi.Join;

import java.util.List;
import java.util.Random;

/**
 * p2c algorithm impl.
 */
@Join
public class P2cLoadBalancer extends AbstractLoadBalancer {

    /**
     * maximum tolerance of idle time.
     */
    private static final int FORCE_GAP = 3 * 1000;

    /**
     * penalty value.
     */
    private static final int PENALTY = 250 * 1000;

    /**
     * pick times.
     */
    private static final int PICK_TIMES = 3;

    private Random random = new Random();

    /**
     * pick of 2 choices to select upstream.
     *
     * @param upstreamList the upstream list
     * @param ip           the ip
     * @return selected upstream
     */
    @Override
    protected Upstream doSelect(final List<Upstream> upstreamList, final String ip) {
        long start = System.currentTimeMillis();
        Upstream[] upstreams = pickTwoUpstreams(upstreamList);
        Upstream picked;
        Upstream unpicked;
        if (load(upstreams[0]) > load(upstreams[1])) {
            picked = upstreams[1];
            unpicked = upstreams[0];
        } else {
            picked = upstreams[0];
            unpicked = upstreams[1];
        }
        // If the failed node is not selected once in the forceGap period, it is forced to be selected once.
        long pick = unpicked.getLastPicked();
        if ((start - pick) > FORCE_GAP) {
            unpicked.setLastPicked(start);
            picked = unpicked;
        }

        if (picked != unpicked) {
            picked.setLastPicked(start);
        }
        picked.getInflight().incrementAndGet();
        return picked;
    }

    /**
     * select two nodes randomly.
     *
     * @param upstreamList the upstream list
     * @return two upstream
     */
    private Upstream[] pickTwoUpstreams(final List<Upstream> upstreamList) {
        Upstream[] upstreams = new Upstream[2];
        for (int i = 0; i < PICK_TIMES; i++) {
            int a = random.nextInt(upstreamList.size());
            int b = random.nextInt(upstreamList.size() - 1);
            // prevent random nodes from being the same.
            if (b >= a) {
                b += 1;
            }
            upstreams[0] = upstreamList.get(a);
            upstreams[1] = upstreamList.get(b);
            if (upstreams[0].isHealthy() && upstreams[1].isHealthy()) {
                break;
            }
        }
        return upstreams;
    }

    /**
     * calculate load.
     *
     * @param upstream the upstream
     * @return load
     */
    public long load(final Upstream upstream) {
        long lag = (long) (Math.sqrt((double) upstream.getLag()) + 1);
        long load = lag * upstream.getInflight().get();
        if (load == 0) {
            load = PENALTY;
        }
        return load;
    }
}
