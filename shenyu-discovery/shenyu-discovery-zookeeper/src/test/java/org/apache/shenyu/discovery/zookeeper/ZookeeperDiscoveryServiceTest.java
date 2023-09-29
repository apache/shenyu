package org.apache.shenyu.discovery.zookeeper;

import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class ZookeeperDiscoveryServiceTest {
    @Test
    public void testDiscoverySPI1() {
        ShenyuDiscoveryService zk = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin("zookeeper");
        assertThat(zk.getClass().getName(), is(ZookeeperDiscoveryService.class.getName()));
    }
}