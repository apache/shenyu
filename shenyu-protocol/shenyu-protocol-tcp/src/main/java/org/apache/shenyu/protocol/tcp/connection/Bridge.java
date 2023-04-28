package org.apache.shenyu.protocol.tcp.connection;

import reactor.netty.Connection;

public interface Bridge {


    /**
     * @param server tcp server connection
     * @param client tcp client connection
     */
    void bridge(final Connection server, final Connection client);
}
