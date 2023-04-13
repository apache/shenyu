package org.apache.shenyu.protocol.tcp.connection;

import org.apache.commons.io.IOUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileInputStream;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Random;

public class ClientConnectionConfigProviderFactory {
    private static final Logger LOG = LoggerFactory.getLogger(ClientConnectionConfigProviderFactory.class);
    public static final Random random = new Random();

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
                    FileInputStream fileInputStream = new FileInputStream("/Users/wenjie.yang/dev/workspace/github/incubator-shenyu/shenyu-protocol/shenyu-protocol-tcp/src/main/resources/config.txt");
                    List<String> strings = IOUtils.readLines(fileInputStream);
                    String s = strings.get(0);
                    String[] split1 = s.split(",");
                    ArrayList<InetSocketAddress> inetSocketAddresses = new ArrayList<>();
                    for (String ss : split1) {
                        String[] split = ss.split(":");
                        String ip = split[0];
                        Integer port = Integer.parseInt(split[1]);
                        LOG.info("success get ip={}|port={}", ip, port);
                        inetSocketAddresses.add(new InetSocketAddress(ip, port));
                    }
                    int idx = random.nextInt(inetSocketAddresses.size());
                    return inetSocketAddresses.get(idx);
                } catch (Exception e) {
                    throw new ShenyuException(e);
                }

            }

            @Override
            public void init(Properties props) {
                System.out.println("props");
            }
        };
    }
}
