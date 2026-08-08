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

import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * The type least activity load balance test.
 */
public class LeastActiveLoadBalanceTest {
    private final List<Upstream> onlyOneList = new ArrayList<>();

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
        onlyOneList.add(upstream1);
        onlyOneList.add(upstream2);
    }

    @Test
    public void testResponseTimeBalancer() throws Exception {
        buildUpstreamList();
        final LeastActiveLoadBalance leastActiveLoadBalance = new LeastActiveLoadBalance();
        Upstream upstream = leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData());
        Upstream upstream1 = leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData());
        Assertions.assertTrue(upstream.getUrl().equals("baidu.com") && upstream1.getUrl().equals("pro.jd.com")
                || upstream1.getUrl().equals("baidu.com") && upstream.getUrl().equals("pro.jd.com"));
    }

    @Test
    public void testRemoveStaleCountMapEntries() throws Exception {
        buildUpstreamList();
        final LeastActiveLoadBalance leastActiveLoadBalance = new LeastActiveLoadBalance();
        leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData());
        Assertions.assertEquals(2, getCountMap(leastActiveLoadBalance).size());
        onlyOneList.remove(1);
        leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData());
        Map<String, Long> countMap = getCountMap(leastActiveLoadBalance);
        Assertions.assertEquals(1, countMap.size());
        Assertions.assertTrue(countMap.containsKey("https://baidu.com"));
        Assertions.assertFalse(countMap.containsKey("https://pro.jd.com"));
    }

    @SuppressWarnings("unchecked")
    private Map<String, Long> getCountMap(final LeastActiveLoadBalance loadBalance) throws Exception {
        Field field = LeastActiveLoadBalance.class.getDeclaredField("countMap");
        field.setAccessible(true);
        return (Map<String, Long>) field.get(loadBalance);
    }
}
