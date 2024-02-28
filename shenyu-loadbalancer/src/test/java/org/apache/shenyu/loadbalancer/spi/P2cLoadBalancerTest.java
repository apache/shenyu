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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

public class P2cLoadBalancerTest {
    private final List<Upstream> upstreamList = new ArrayList<>();

    /**
     * build upstream list.
     */
    public void buildUpstreamList() {
        Upstream upstream1 = Upstream.builder()
                .url("baidu.com")
                .protocol("https://")
                .build();
        Upstream upstream2 = Upstream.builder()
                .url("pro.jd.com")
                .protocol("https://")
                .build();
        upstreamList.add(upstream1);
        upstreamList.add(upstream2);
    }

    @Test
    public void testResponseTimeBalancerSameLag() {
        buildUpstreamList();
        final P2cLoadBalancer p2cLoadBalancer = new P2cLoadBalancer();
        Upstream upstream = p2cLoadBalancer.doSelect(upstreamList, "localhost");
        Upstream upstream1 = p2cLoadBalancer.doSelect(upstreamList, "localhost");
        Assertions.assertTrue((upstream.getUrl().equals("baidu.com") && upstream1.getUrl().equals("pro.jd.com"))
                || upstream1.getUrl().equals("baidu.com") && upstream.getUrl().equals("pro.jd.com"));
    }

    @Test
    public void testResponseTimeBalancerSameInflight() {
        buildUpstreamList();
        final P2cLoadBalancer p2cLoadBalancer = new P2cLoadBalancer();
        upstreamList.get(0).setLag(1);
        Upstream upstream = p2cLoadBalancer.doSelect(upstreamList, "localhost");
        Upstream upstream1 = p2cLoadBalancer.doSelect(upstreamList, "localhost");
        Assertions.assertTrue(upstream.getUrl().equals("baidu.com") && upstream1.getUrl().equals("pro.jd.com"));
    }
}
