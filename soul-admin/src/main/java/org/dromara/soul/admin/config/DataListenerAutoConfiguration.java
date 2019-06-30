package org.dromara.soul.admin.config;

import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.admin.listener.DataChangedListener;
import org.dromara.soul.admin.listener.http.HttpLongPollingDataChangedListener;
import org.dromara.soul.admin.listener.zookeeper.ZookeeperDataChangedListener;
import org.dromara.soul.configuration.zookeeper.ZookeeperConfig;
import org.dromara.soul.configuration.zookeeper.ZookeeperConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * Admin data notification configuration.
 * @author huangxiaofeng
 * @date 2019/6/26 14:40
 */
@Configuration
public class DataListenerAutoConfiguration {

    /**
     * zookeeper
     */
    @Configuration
    @ConditionalOnMissingBean(DataChangedListener.class)
    @ConditionalOnProperty(name = "soul.admin.notify.type", havingValue = "zookeeper")
    @Import(ZookeeperConfiguration.class)
    static class Zookeeper {
        @Bean
        public DataChangedListener configEventListener(ZkClient zkClient) {
            return new ZookeeperDataChangedListener(zkClient);
        }

        @Bean
        @ConfigurationProperties(prefix = "soul.admin.notify.zookeeper")
        public ZookeeperConfig zookeeperConfig() {
            return new ZookeeperConfig();
        }
    }

    /**
     * http long polling(default strategy)
     */
    @Configuration
    @ConditionalOnMissingBean(DataChangedListener.class)
    @ConditionalOnProperty(name = "soul.admin.notify.type", havingValue = "long-poll", matchIfMissing = true)
    static class HttpLongPolling {

        @Bean
        @ConfigurationProperties(prefix = "soul.admin.notify.long-poll")
        public HttpLongPollingDataChangedListener configEventListener() {
            return new HttpLongPollingDataChangedListener();
        }

    }

}
