package org.apache.shenyu.protocol.tcp.connection;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.protocol.tcp.UpstreamProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.*;
import java.util.List;

/**
 * ClientConnectionConfigProviderFactory.
 */
public class DefaultConnectionConfigProvider implements ClientConnectionConfigProvider {
    private static final Logger LOG = LoggerFactory.getLogger(DefaultConnectionConfigProvider.class);
    private final String loadBalanceAlgorithm;
    private volatile List<Upstream> discoveryUpstreams;


    public DefaultConnectionConfigProvider(final String loadBalanceAlgorithm, final String pluginSelectorName) {
        this.loadBalanceAlgorithm = loadBalanceAlgorithm;
        discoveryUpstreams = UpstreamProvider.getSingleton().provide(pluginSelectorName);
    }

    @Override
    public URI getProxiedService(String ip) {
        Upstream upstream = LoadBalancerFactory.selector(discoveryUpstreams, loadBalanceAlgorithm, ip);
        return cover(upstream);
    }

    private URI cover(Upstream upstream) {
        try {
            return new URI(upstream.getProtocol() + "://" + upstream.getUrl());
        } catch (URISyntaxException e) {
            LOG.error("Upstream url is wrong", e);
            throw new ShenyuException(e);
        }
    }

}
