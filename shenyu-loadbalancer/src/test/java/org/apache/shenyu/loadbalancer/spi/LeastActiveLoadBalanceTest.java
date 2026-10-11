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
        // The periodic cleanup is time-driven, simulate the elapsed time so that the entry of the
        // removed upstream becomes stale and is evicted by the next cleanup.
        long old = System.currentTimeMillis() - 60_001;
        setLastUpdate(leastActiveLoadBalance, "https://pro.jd.com", old);
        setLastRecycle(leastActiveLoadBalance, old);
        leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData());
        Map<String, LeastActiveLoadBalance.ActiveCount> countMap = getCountMap(leastActiveLoadBalance);
        Assertions.assertEquals(1, countMap.size());
        Assertions.assertTrue(countMap.containsKey("https://baidu.com"));
        Assertions.assertFalse(countMap.containsKey("https://pro.jd.com"));
    }

    @Test
    public void testCountMapNotAffectOtherSelector() throws Exception {
        buildUpstreamList();
        final List<Upstream> anotherList = new ArrayList<>();
        anotherList.add(Upstream.builder().url("jd.com").protocol("https://").build());
        final LeastActiveLoadBalance leastActiveLoadBalance = new LeastActiveLoadBalance();
        leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData());
        leastActiveLoadBalance.doSelect(anotherList, new LoadBalanceData());
        Assertions.assertEquals(3, getCountMap(leastActiveLoadBalance).size());
        // Selector 1 removes pro.jd.com, and the cleanup must not evict the live entry jd.com
        // which only belongs to selector 2.
        onlyOneList.remove(1);
        long old = System.currentTimeMillis() - 60_001;
        setLastUpdate(leastActiveLoadBalance, "https://pro.jd.com", old);
        setLastRecycle(leastActiveLoadBalance, old);
        leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData());
        Map<String, LeastActiveLoadBalance.ActiveCount> countMap = getCountMap(leastActiveLoadBalance);
        Assertions.assertEquals(2, countMap.size());
        Assertions.assertTrue(countMap.containsKey("https://baidu.com"));
        Assertions.assertTrue(countMap.containsKey("https://jd.com"));
        Assertions.assertFalse(countMap.containsKey("https://pro.jd.com"));
    }

    @Test
    public void testCountMapNotAffectOtherSelectorWhenFirstRemoved() throws Exception {
        buildUpstreamList();
        final List<Upstream> anotherList = new ArrayList<>();
        anotherList.add(Upstream.builder().url("jd.com").protocol("https://").build());
        final LeastActiveLoadBalance leastActiveLoadBalance = new LeastActiveLoadBalance();
        leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData());
        leastActiveLoadBalance.doSelect(anotherList, new LoadBalanceData());
        Assertions.assertEquals(3, getCountMap(leastActiveLoadBalance).size());
        // Symmetric case: selector 1 removes the first upstream baidu.com. The cleanup must evict
        // baidu.com but keep pro.jd.com (still served by selector 1) and jd.com (served by selector 2).
        onlyOneList.remove(0);
        long old = System.currentTimeMillis() - 60_001;
        setLastUpdate(leastActiveLoadBalance, "https://baidu.com", old);
        setLastRecycle(leastActiveLoadBalance, old);
        leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData());
        Map<String, LeastActiveLoadBalance.ActiveCount> countMap = getCountMap(leastActiveLoadBalance);
        Assertions.assertEquals(2, countMap.size());
        Assertions.assertTrue(countMap.containsKey("https://pro.jd.com"));
        Assertions.assertTrue(countMap.containsKey("https://jd.com"));
        Assertions.assertFalse(countMap.containsKey("https://baidu.com"));
    }

    @SuppressWarnings("unchecked")
    private Map<String, LeastActiveLoadBalance.ActiveCount> getCountMap(final LeastActiveLoadBalance loadBalance) throws Exception {
        Field field = LeastActiveLoadBalance.class.getDeclaredField("countMap");
        field.setAccessible(true);
        return (Map<String, LeastActiveLoadBalance.ActiveCount>) field.get(loadBalance);
    }

    private void setLastUpdate(final LeastActiveLoadBalance loadBalance, final String domain, final long lastUpdate) throws Exception {
        LeastActiveLoadBalance.ActiveCount activeCount = getCountMap(loadBalance).get(domain);
        Assertions.assertNotNull(activeCount);
        Field field = LeastActiveLoadBalance.ActiveCount.class.getDeclaredField("lastUpdate");
        field.setAccessible(true);
        field.set(activeCount, lastUpdate);
    }

    private void setLastRecycle(final LeastActiveLoadBalance loadBalance, final long lastRecycle) throws Exception {
        Field field = LeastActiveLoadBalance.class.getDeclaredField("lastRecycle");
        field.setAccessible(true);
        field.set(loadBalance, lastRecycle);
    }
}
