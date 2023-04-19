package org.apache.shenyu.protocol.tcp.connection;

import org.apache.commons.io.IOUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.DiscoveryUpstream;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.listener.DataChangedEvent;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.spi.LoadBalancer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileInputStream;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Random;

/**
 * ClientConnectionConfigProviderFactory.
 */
public class DefaultConnectionConfigProvider implements ClientConnectionConfigProvider {
    private static final Logger LOG = LoggerFactory.getLogger(DefaultConnectionConfigProvider.class);
    private final ShenyuDiscoveryService shenyuDiscoveryService;
    private final LoadBalancer loadBalancer;

    private volatile List<Upstream> discoveryUpstreams;

    public DefaultConnectionConfigProvider(ShenyuDiscoveryService shenyuDiscoveryService, LoadBalancer loadBalancer) {
        this.loadBalancer = loadBalancer;
        this.shenyuDiscoveryService = shenyuDiscoveryService;
    }



    @Override
    public InetSocketAddress getProxiedService() {
        Upstream ip = loadBalancer.select(discoveryUpstreams, "ip");
        return cover(ip);
    }

    @Override
    public void init(Properties props) {
        String key = props.getProperty("key");
        String data = shenyuDiscoveryService.getData(key);
        discoveryUpstreams = GsonUtils.getInstance().fromCurrentList(data, Upstream.class);
        shenyuDiscoveryService.watcher(key, event -> {
            //impl
            switch (event.getEvent()){
                case UPDATED:


            }
        });

    }

    private InetSocketAddress cover(Upstream ip) {
        return new InetSocketAddress(90);
    }

}
