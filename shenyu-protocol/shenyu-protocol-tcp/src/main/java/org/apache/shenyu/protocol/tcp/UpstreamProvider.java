package org.apache.shenyu.protocol.tcp;

import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class UpstreamProvider {
    private Map<String, List<Upstream>> chache = new ConcurrentHashMap<>();

    private UpstreamProvider() {
    }

    private final static UpstreamProvider SINGLETON = new UpstreamProvider();

    public static UpstreamProvider getSingleton() {
        return SINGLETON;
    }

    public List<Upstream> provide(String pluginSelectorName) {
        return chache.get(pluginSelectorName);
    }

    public void createUpstreams(String pluginSelectorName, List<Upstream> upstreams) {
        chache.put(pluginSelectorName, upstreams);
    }

    public void refresh(String pluginSelectorName, List<Upstream> upstreams) {
        List<Upstream> remove = chache.remove(pluginSelectorName);
        remove = null;
        chache.put(pluginSelectorName, upstreams);
    }
}
