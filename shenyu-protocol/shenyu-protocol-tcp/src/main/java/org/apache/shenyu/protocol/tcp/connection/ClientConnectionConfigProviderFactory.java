package org.apache.shenyu.protocol.tcp.connection;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.net.InetSocketAddress;
import java.util.List;
import java.util.Properties;

public class ClientConnectionConfigProviderFactory {
    private static final Logger LOG = LoggerFactory.getLogger(ClientConnectionConfigProviderFactory.class);

    private ClientConnectionConfigProviderFactory() {
    }

    private static final ClientConnectionConfigProviderFactory INSRANCE = new ClientConnectionConfigProviderFactory();

    public static ClientConnectionConfigProviderFactory getInstance() {
        return INSRANCE;
    }

    public ClientConnectionConfigProvider getClientConnectionConfigProviderByType(SyancType syancType) {
        //todo impl it
        return new ClientConnectionConfigProvider() {
            @Override
            public InetSocketAddress getProxiedService() {

                try {
                    InputStream resourceAsStream = this.getClass().getResourceAsStream("/config.txt");
                    List<String> strings = IOUtils.readLines(resourceAsStream);
                    String s = strings.get(0);
                    String[] split = s.split(":");
                    String ip = split[0];
                    Integer port = Integer.parseInt(split[1]);
                    LOG.info("success get ip={}|port={}", ip, port);
                    return new InetSocketAddress(ip, port);
                } catch (Exception e) {
                    throw new RuntimeException("erropr");
                }

            }

            @Override
            public void init(Properties props) {
                System.out.println("props");
            }
        };
    }
}
