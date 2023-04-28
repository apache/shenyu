package org.apache.shenyu.protocol.tcp.connection;

import java.net.URI;

/**
 * ClientConnectionConfigProvider.
 */
public interface ClientConnectionConfigProvider {

    /**
     * getProxiedService.
     *
     * @return InetSocketAddress.
     */
    URI getProxiedService(String ip);

}
