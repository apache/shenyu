package org.apache.shenyu.protocol.tcp.connection;

import java.net.InetSocketAddress;
import java.util.Properties;

public interface ClientConnectionConfigProvider {

    InetSocketAddress getProxiedService();

    void init(Properties props);

}
