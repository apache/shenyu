package org.apache.shenyu.plugin.tcp.handler;

import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.protocol.tcp.BootstrapServer;
import org.apache.shenyu.protocol.tcp.TcpServerConfiguration;
import org.apache.shenyu.protocol.tcp.UpstreamProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * upstreamList data change
 */
public class TcpUpstreamDataHandler {
    private static final Logger LOG = LoggerFactory.getLogger(TcpUpstreamDataHandler.class);

    private final Map<String, BootstrapServer> cache = new ConcurrentHashMap<>();

    public void handlerPlugin(final ProxySelectorData proxySelectorData, final Action action, final List<Upstream> upstreamList) {
        String name = proxySelectorData.getName();
        switch (action) {
            case CREATE:
                Integer forwardPort = proxySelectorData.getForwardPort();
                TcpServerConfiguration tcpServerConfiguration = new TcpServerConfiguration();
                tcpServerConfiguration.setPort(forwardPort);
                tcpServerConfiguration.setProps(proxySelectorData.getProps());
                tcpServerConfiguration.setPluginSelectorName(name);
                UpstreamProvider.getSingleton().createUpstreams(name, upstreamList);
                BootstrapServer bootstrapServer = TcpBootstrapFactory.getSingleton().createBootstrapServer(tcpServerConfiguration);
                cache.put(name, bootstrapServer);
                LOG.info("shenyu create TcpBootstrapServer success port is {}", forwardPort);
                break;
            case UPDATE:
                UpstreamProvider.getSingleton().refresh(name, upstreamList);
                break;
            case DELETE:
                cache.remove(name).shutdown();
                break;
        }

    }

    enum Action {
        CREATE,
        UPDATE,
        DELETE
    }

    public static void main(String[] args) throws IOException {
        ProxySelectorData proxySelectorData = new ProxySelectorData();
        proxySelectorData.setId("1");
        proxySelectorData.setForwardPort(9500);
        proxySelectorData.setName("demo-selector");
        TcpUpstreamDataHandler tcpUpstreamDataHandler = new TcpUpstreamDataHandler();
        Upstream build = Upstream.builder().protocol("tcp").url("127.0.0.1:9095").build();
        tcpUpstreamDataHandler.handlerPlugin(proxySelectorData, Action.CREATE, Arrays.asList(build));
        System.in.read();
    }

}
