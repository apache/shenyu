package org.apache.shenyu.infra.zookeeper.autoconfig;

import org.apache.curator.framework.CuratorFrameworkFactory;
import org.apache.shenyu.infra.zookeeper.client.ZookeeperClient;
import org.apache.shenyu.infra.zookeeper.config.ZookeeperConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Objects;

@Configuration
@ConditionOnSyncZk
@EnableConfigurationProperties(ZookeeperProperties.class)
@ConditionalOnClass(CuratorFrameworkFactory.Builder.class)
public class ZookeeperConfiguration {

    /**
     * register ZookeeperClient in spring ioc.
     *
     * @param zookeeperProp the zookeeper configuration
     * @return ZookeeperClient {@linkplain ZookeeperClient}
     */
    @Bean
    @ConditionalOnMissingBean(ZookeeperClient.class)
    public ZookeeperClient zookeeperClient(final ZookeeperProperties zookeeperProp) {

        int sessionTimeout = Objects.isNull(zookeeperProp.getSessionTimeout()) ? 3000 : zookeeperProp.getSessionTimeout();
        int connectionTimeout = Objects.isNull(zookeeperProp.getConnectionTimeout()) ? 3000 : zookeeperProp.getConnectionTimeout();

        ZookeeperConfig zkConfig = ZookeeperConfig.builder().serverLists(zookeeperProp.getUrl()).build();
        zkConfig.setSessionTimeoutMilliseconds(sessionTimeout)
                .setConnectionTimeoutMilliseconds(connectionTimeout);

        ZookeeperClient client = ZookeeperClient.builder().config(zkConfig).build();
        client.start();

        return client;
    }

}
