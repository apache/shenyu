package org.apache.shenyu.plugin.tcp.handler;

import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.protocol.tcp.TcpServerConfiguration;

public class TcpPluginConfig {

    private TcpServerConfiguration tcpServerConfiguration;

    private DiscoveryConfig discoveryConfig;

    public TcpServerConfiguration getTcpServerConfiguration() {
        return tcpServerConfiguration;
    }

    public void setTcpServerConfiguration(final TcpServerConfiguration tcpServerConfiguration) {
        this.tcpServerConfiguration = tcpServerConfiguration;
    }

    public DiscoveryConfig getDiscoveryConfig() {
        return discoveryConfig;
    }

    public void setDiscoveryConfig(final DiscoveryConfig discoveryConfig) {
        this.discoveryConfig = discoveryConfig;
    }
}
