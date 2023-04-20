package org.apache.shenyu.protocol.tcp.connection;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.DiscoveryUpstream;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Properties;

/**
 * ClientConnectionConfigProviderFactory.
 */
public class DefaultConnectionConfigProvider implements ClientConnectionConfigProvider {
    private static final Logger LOG = LoggerFactory.getLogger(DefaultConnectionConfigProvider.class);
    private final ShenyuDiscoveryService shenyuDiscoveryService;
    private final String loadBalanceAlgorithm;
    private volatile List<Upstream> discoveryUpstreams;


    public DefaultConnectionConfigProvider(ShenyuDiscoveryService shenyuDiscoveryService, String loadBalanceAlgorithm) {
        this.shenyuDiscoveryService = shenyuDiscoveryService;
        this.loadBalanceAlgorithm = loadBalanceAlgorithm;
    }

    @Override
    public URL getProxiedService(String ip) {
        Upstream upstream = LoadBalancerFactory.selector(discoveryUpstreams, loadBalanceAlgorithm, ip);
        return cover(upstream);
    }

    @Override
    public void init(Properties props) {
        String key = props.getProperty("key");
        String data = shenyuDiscoveryService.getData(key);
        discoveryUpstreams = GsonUtils.getInstance().fromCurrentList(data, Upstream.class);
        //todo 待确认
        shenyuDiscoveryService.watcher(key, event -> {
            switch (event.getEvent()) {
                case UPDATED:
                    String value = event.getValue();
                    discoveryUpstreams = GsonUtils.getInstance().fromCurrentList(value, Upstream.class);
                    break;
                case DELETED:
                case ADDED:
                case IGNORED:
                default:
                    break;
            }
        });

    }

    private URL cover(Upstream upstream) {
        try {
            return new URL(upstream.getUrl());
        } catch (MalformedURLException e) {
            LOG.error("Upstream url is wrong", e);
            throw new ShenyuException(e);
        }
    }

}
