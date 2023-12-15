package org.apache.springboot.starter.client.grpc;

import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.register.ClientDiscoveryConfigRefreshedEventListener;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.InstanceRegisterListener;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.client.http.HttpClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ImportAutoConfiguration(ShenyuClientCommonBeanConfiguration.class)
public class ShenyuGrpcDiscoveryConfiguration {

    /**
     * InstanceRegisterListener.
     *
     * @param clientRegisterConfig  clientRegisterConfig
     * @param shenyuDiscoveryConfig shenyuDiscoveryConfig
     * @return InstanceRegisterListener
     */
    @Bean("grpcInstanceRegisterListener")
    @ConditionalOnBean(ShenyuDiscoveryConfig.class)
    @ConditionalOnMissingBean(name = "websocketInstanceRegisterListener")
    public InstanceRegisterListener instanceRegisterListener(final ClientRegisterConfig clientRegisterConfig, final ShenyuDiscoveryConfig shenyuDiscoveryConfig) {
        DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
        discoveryUpstreamData.setUrl(clientRegisterConfig.getHost() + ":" + clientRegisterConfig.getPort());
        discoveryUpstreamData.setStatus(0);
        String weight = shenyuDiscoveryConfig.getProps().getOrDefault("discoveryUpstream.weight", "10").toString();
        discoveryUpstreamData.setWeight(Integer.parseInt(weight));
        discoveryUpstreamData.setProtocol(shenyuDiscoveryConfig.getProps().getOrDefault("discoveryUpstream.protocol", ShenyuClientConstants.HTTP).toString());
        return new InstanceRegisterListener(discoveryUpstreamData, shenyuDiscoveryConfig);
    }

    /**
     * clientDiscoveryConfigRefreshedEventListener.
     *
     * @param shenyuDiscoveryConfig        shenyuDiscoveryConfig
     * @param httpClientRegisterRepository httpClientRegisterRepository
     * @param clientRegisterConfig         clientRegisterConfig
     * @return ClientDiscoveryConfigRefreshedEventListener
     */
    @Bean("GrpcClientDiscoveryConfigRefreshedEventListener")
    @ConditionalOnProperty(prefix = "shenyu.discovery", name = "serverList", matchIfMissing = false)
    @ConditionalOnBean(ShenyuDiscoveryConfig.class)
    public ClientDiscoveryConfigRefreshedEventListener clientDiscoveryConfigRefreshedEventListener(final ShenyuDiscoveryConfig shenyuDiscoveryConfig,
                                                                                                   final HttpClientRegisterRepository httpClientRegisterRepository,
                                                                                                   final ClientRegisterConfig clientRegisterConfig) {
        return new ClientDiscoveryConfigRefreshedEventListener(shenyuDiscoveryConfig, httpClientRegisterRepository, clientRegisterConfig, PluginEnum.GRPC);
    }

}
