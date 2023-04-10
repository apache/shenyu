package org.apache.shenyu.protocol.tcp.connection;

import java.net.InetSocketAddress;
import java.util.Properties;

public class ClientConnectionConfigProviderFactory {

    private ClientConnectionConfigProviderFactory(){}

    private static final ClientConnectionConfigProviderFactory  INSRANCE = new ClientConnectionConfigProviderFactory();

    public static ClientConnectionConfigProviderFactory getInstance() {
        return INSRANCE;
    }

    public ClientConnectionConfigProvider getClientConnectionConfigProviderByType(SyancType syancType) {
        //todo impl it
        return new ClientConnectionConfigProvider() {
            @Override
            public InetSocketAddress getProxiedService() {
                return new InetSocketAddress("127.0.0.1",9095);
            }

            @Override
            public void init(Properties props) {
                System.out.println("props");
            }
        };
    }
}
