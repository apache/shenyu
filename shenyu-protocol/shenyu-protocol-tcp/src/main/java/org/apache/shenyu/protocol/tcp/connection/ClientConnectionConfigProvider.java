package org.apache.shenyu.protocol.tcp.connection;

import java.net.InetSocketAddress;
import java.net.URL;
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
    URL getProxiedService(String ip);

    /**
     * init.
     *
     * @param props props
     */
    void init(Properties props);

}
