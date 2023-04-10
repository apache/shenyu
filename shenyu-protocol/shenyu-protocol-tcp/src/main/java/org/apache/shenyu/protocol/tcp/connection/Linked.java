package org.apache.shenyu.protocol.tcp.connection;

import reactor.netty.Connection;

public interface Linked {


    /**
     * @param server tcp server connection
     * @param client tcp client connection
     */
    public void link(final Connection server, final Connection client);
}
