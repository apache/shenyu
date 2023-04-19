package org.apache.shenyu.protocol.tcp.connection;

import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.InetSocketAddress;
import java.util.List;
import java.util.Properties;

/**
 * ClientConnectionConfigProviderFactory.
 */
public class DefaultConnectionConfigProvider implements ClientConnectionConfigProvider {
    private static final Logger LOG = LoggerFactory.getLogger(DefaultConnectionConfigProvider.class);
    private final ShenyuDiscoveryService shenyuDiscoveryService;

    private volatile List<Upstream> discoveryUpstreams;

    public DefaultConnectionConfigProvider(ShenyuDiscoveryService shenyuDiscoveryService) {
        this.shenyuDiscoveryService = shenyuDiscoveryService;
    }



    @Override
    public InetSocketAddress getProxiedService(String ip) {
        Upstream upstream = LoadBalancerFactory.selector(discoveryUpstreams, "random", ip);
        return cover(upstream);
    }

    @Override
    public void init(Properties props) {
        String key = props.getProperty("key");
        String data = shenyuDiscoveryService.getData(key);
        discoveryUpstreams = GsonUtils.getInstance().fromCurrentList(data, Upstream.class);
        shenyuDiscoveryService.watcher(key, event -> {
            //todo impl
            switch (event.getEvent()){
                case UPDATED:


            }
        });

    }

    private InetSocketAddress cover(Upstream ip) {
        return new InetSocketAddress(90);
    }

}
