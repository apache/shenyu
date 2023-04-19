package org.apache.shenyu.protocol.tcp.connection;

import java.net.InetSocketAddress;
import java.util.Properties;

/**
 * ClientConnectionConfigProvider.
 */
public interface ClientConnectionConfigProvider {

    /**
     * getProxiedService.
     *
     * @return InetSocketAddress.
     */
    InetSocketAddress getProxiedService(String ip );

    /**
     * init.
     *
     * @param props props
     */
    void init(Properties props);

}
