package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.etcd.EtcdDiscoveryService;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public class test2 {

    @Test
    public void testDiscoverySPIadmin2() {
        ShenyuDiscoveryService etcd = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin("etcd");
        assertThat(etcd.getClass().getName(), is(EtcdDiscoveryService.class.getName()));
    }

}
