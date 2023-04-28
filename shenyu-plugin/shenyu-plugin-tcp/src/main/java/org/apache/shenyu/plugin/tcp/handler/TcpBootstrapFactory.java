package org.apache.shenyu.plugin.tcp.handler;

import org.apache.shenyu.protocol.tcp.BootstrapServer;
import org.apache.shenyu.protocol.tcp.TcpServerConfiguration;


public class TcpBootstrapFactory {
    private TcpBootstrapFactory() {
    }

    private static final TcpBootstrapFactory SINGLETON = new TcpBootstrapFactory();

    public static TcpBootstrapFactory getSingleton() {
        return SINGLETON;
    }

    public BootstrapServer createBootstrapServer(TcpServerConfiguration configuration) {
        BootstrapServer bootstrapServer = new BootstrapServer();
        bootstrapServer.start(configuration);
        return bootstrapServer;
    }


}
