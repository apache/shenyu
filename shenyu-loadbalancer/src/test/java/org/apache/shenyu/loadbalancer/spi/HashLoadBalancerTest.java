package org.apache.shenyu.loadbalancer.spi;

import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * HashLoadBalancer unit test
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