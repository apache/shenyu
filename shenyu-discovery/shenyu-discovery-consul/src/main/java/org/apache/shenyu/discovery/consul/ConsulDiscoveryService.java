package org.apache.shenyu.discovery.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.agent.model.NewService;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ConsulDiscoveryService implements ShenyuDiscoveryService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConsulDiscoveryService.class);

    private ConsulClient consulClient;

    private NewService newService;

    private final Map<String, Long> consulIndexes = new HashMap<>();

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
