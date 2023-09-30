package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.zookeeper.ZookeeperDiscoveryService;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public class test1 {
    @Test
    public void testDiscoverySPIadmin() {
        ShenyuDiscoveryService zk = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin("zookeeper");
        assertThat(zk.getClass().getName(), is(ZookeeperDiscoveryService.class.getName()));
    }
}
