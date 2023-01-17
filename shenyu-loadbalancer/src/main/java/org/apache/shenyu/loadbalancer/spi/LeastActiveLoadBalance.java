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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.spi.Join;

/**
 * least active algorithm impl.
 */
@Join
public class LeastActiveLoadBalance extends AbstractLoadBalancer {

    private Map<String, Integer> countMap = new ConcurrentHashMap<>();

    @Override
    protected Upstream doSelect(final List<Upstream> upstreamList, final String ip) {
        Map<String, Upstream> domainList = new ConcurrentHashMap<>();
        upstreamList.stream().forEach(upstream -> {
            domainList.put(upstream.buildDomain(), upstream);
        });
        countMap.keySet().forEach(key -> {
            if (!domainList.keySet().contains(key)) {
                countMap.remove(key);
            }
        });
        for (Upstream upstream:upstreamList) {
            if (countMap.get(upstream.buildDomain()) == null) {
                countMap.put(upstream.buildDomain(), 1);
                return upstream;
            } else {
                countMap.put(upstream.buildDomain(), countMap.get(upstream.buildDomain()) + 1);
            }
        }
        AtomicReference<String> leastDomainUrl = new AtomicReference<>(upstreamList.get(0).buildDomain());
        countMap.keySet().forEach(key -> {
            if (countMap.get(key) < countMap.get(leastDomainUrl)) {
                leastDomainUrl.set(key);
            }
        });
        return domainList.get(leastDomainUrl);
    }
    
}
