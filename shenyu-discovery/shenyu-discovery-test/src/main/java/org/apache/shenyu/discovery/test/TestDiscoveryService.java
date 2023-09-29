package org.apache.shenyu.discovery.test;

import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.spi.Join;

import java.util.List;

@Join
public class TestDiscoveryService implements ShenyuDiscoveryService {
    @Override
    public void init(DiscoveryConfig config) {

    }

    @Override
    public void watch(String key, DataChangedEventListener listener) {

    }

    @Override
    public void unwatch(String key) {

    }

    @Override
    public void register(String key, String value) {

    }

    @Override
    public List<String> getRegisterData(String key) {
        return null;
    }

    @Override
    public Boolean exists(String key) {
        return null;
    }

    @Override
    public void shutdown() {

    }
}
