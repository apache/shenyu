package org.apache.shenyu.discovery.etcd;

import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class EtcdDiscoveryServiceTest {

    @Test
    public void testDiscoverySPI1() {
        ShenyuDiscoveryService etcd = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin("etcd");
        assertThat(etcd.getClass().getName(), is(EtcdDiscoveryService.class.getName()));
    }



}