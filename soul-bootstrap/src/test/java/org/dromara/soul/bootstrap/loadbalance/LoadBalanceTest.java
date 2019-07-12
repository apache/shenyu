package org.dromara.soul.bootstrap.loadbalance;

import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.web.balance.spi.RoundRobinLoadBalance;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author wanglaomo
 * @since 2019/7/12
 **/
public class LoadBalanceTest {

    @Test
    public void roundRobinLoadBalanceTest() {

        List<DivideUpstream> divideUpstreamList =
                Arrays.asList(50, 20, 30).stream()
                        .map(weight -> {
                            DivideUpstream divideUpstream = new DivideUpstream();
                            divideUpstream.setUpstreamUrl("divide-upstream-" + weight);
                            divideUpstream.setWeight(weight);
                            return divideUpstream;
                        })
                        .collect(Collectors.toList());

        RoundRobinLoadBalance roundRobinLoadBalance = new RoundRobinLoadBalance();
        Map<String, Integer> countMap = new HashMap<>();
        for(int i=0; i<100; i++) {
            DivideUpstream result = roundRobinLoadBalance.select(divideUpstreamList, "");
            int count = countMap.getOrDefault(result.getUpstreamUrl(), 0);
            countMap.put(result.getUpstreamUrl(), ++count);
        }

        Assert.assertEquals(50, countMap.get("divide-upstream-50").intValue());
    }
}
