package org.apache.shenyu.discovery.test;

import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class TestDiscoveryServiceTest {
    @Test
    public void testDiscoverySPI6() {
        ShenyuDiscoveryService tst = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin("test");
        assertThat(tst.getClass().getName(), is(TestDiscoveryService.class.getName()));
    }
}