package org.apache.shenyu.infra.etcd;

import io.etcd.jetcd.Client;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConditionalOnProperty(prefix = "shenyu.sync.etcd", name = "url")
public class EtcdConfiguration {

    /**
     * Init etcd client.
     *
     * @param etcdProperties etcd properties
     * @return Etcd Client
     */
    @Bean
    public EtcdClient etcdClient(final EtcdProperties etcdProperties) {

        Client client = Client.builder()
                .endpoints(etcdProperties.getUrl().split(","))
                .build();
        return new EtcdClient(client);
    }

}
