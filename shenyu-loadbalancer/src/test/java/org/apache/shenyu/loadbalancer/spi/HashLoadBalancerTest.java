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
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * HashLoadBalancer unit test.
 */
class HashLoadBalancerTest {

    @Test
    void doSelectWithSuccess() {
        final HashLoadBalancer hashLoadBalancer = new HashLoadBalancer();
        final List<Upstream> upstreamList = new ArrayList<>();
        upstreamList.add(Upstream.builder().url("http://1.1.1.1/api").build());
        upstreamList.add(Upstream.builder().url("http://2.2.2.2/api").build());
        upstreamList.add(Upstream.builder().url("http://3.3.3.3/api").build());

        final Upstream upstream = hashLoadBalancer.doSelect(upstreamList, "127.0.0.1");
        assertEquals(upstreamList.get(2).getUrl(), upstream.getUrl());
    }

}
