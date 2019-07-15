package org.dromara.soul.bootstrap.loadbalance;

import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.web.balance.spi.RoundRobinLoadBalance;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type Load balance test.
 *
 * @author wanglaomo
 */
public class LoadBalanceTest {

    /**
     * Round robin load balance test.
     */
    @Test
    public void roundRobinLoadBalanceTest() {

        List<DivideUpstream> divideUpstreamList =
                Stream.of(50, 20, 30)
                        .map(weight -> {
                            DivideUpstream divideUpstream = new DivideUpstream();
                            divideUpstream.setUpstreamUrl("divide-upstream-" + weight);
                            divideUpstream.setWeight(weight);
                            return divideUpstream;
                        })
                        .collect(Collectors.toList());

        RoundRobinLoadBalance roundRobinLoadBalance = new RoundRobinLoadBalance();
        Map<String, Integer> countMap = new HashMap<>();
        for (int i = 0; i < 120; i++) {
            DivideUpstream result = roundRobinLoadBalance.select(divideUpstreamList, "");
            int count = countMap.getOrDefault(result.getUpstreamUrl(), 0);
            countMap.put(result.getUpstreamUrl(), ++count);
        }

        Assert.assertEquals(50, countMap.get("divide-upstream-50").intValue());
    }
}
