package org.apache.shenyu.discovery.etcd;

import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

class EtcdDiscoveryServiceTest {
    @Test
    public void testDiscoverySPI1() {
        ShenyuDiscoveryService zk = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin("etcd");
        assertThat(zk.getClass().getName(), is(EtcdDiscoveryService.class.getName()));
    }
}