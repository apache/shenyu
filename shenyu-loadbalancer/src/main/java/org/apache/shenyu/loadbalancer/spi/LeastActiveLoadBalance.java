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

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.spi.Join;

/**
 * least active algorithm impl.
 */
@Join
public class LeastActiveLoadBalance extends AbstractLoadBalancer {

    private final Map<String, Long> countMap = new ConcurrentHashMap<>();

    @Override
    protected Upstream doSelect(final List<Upstream> upstreamList, final String ip) {
        Map<String, Upstream> domainMap = upstreamList.stream()
                .collect(Collectors.toConcurrentMap(Upstream::buildDomain, upstream -> upstream));

        domainMap.keySet().stream()
                .filter(key -> !countMap.containsKey(key))
                .forEach(domain -> countMap.put(domain, Long.MIN_VALUE));

        final String domain = countMap.entrySet().stream()
                // Ensure that the filtered domain is included in the domainMap.
                .filter(entry -> domainMap.containsKey(entry.getKey()))
                .min(Comparator.comparingLong(Map.Entry::getValue))
                .map(Map.Entry::getKey)
                .orElse(upstreamList.get(0).buildDomain());

        countMap.computeIfPresent(domain, (key, actived) -> Optional.of(actived).orElse(Long.MIN_VALUE) + 1);
        return domainMap.get(domain);
    }
    
}
