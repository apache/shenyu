package org.apache.shenyu.springboot.starter.client.spring.websocket;

import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.register.ClientDiscoveryConfigRefreshedEventListener;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.InstanceRegisterListener;
import org.apache.shenyu.client.spring.websocket.init.SpringWebSocketClientEventListener;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.client.http.HttpClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ImportAutoConfiguration(ShenyuClientCommonBeanConfiguration.class)
public class ShenyuSpringWebSocketDiscoveryConfiguration {

    /**
     * clientDiscoveryConfigRefreshedEventListener.
     *
     * @param shenyuDiscoveryConfig        shenyuDiscoveryConfig
     * @param httpClientRegisterRepository httpClientRegisterRepository
     * @param clientRegisterConfig         clientRegisterConfig
     * @return ClientDiscoveryConfigRefreshedEventListener
     */
    @Bean("WebSocketClientDiscoveryConfigRefreshedEventListener")
    @ConditionalOnProperty(prefix = "shenyu.discovery", name = "serverList", matchIfMissing = false)
    @ConditionalOnBean(ShenyuDiscoveryConfig.class)
    public ClientDiscoveryConfigRefreshedEventListener clientDiscoveryConfigRefreshedEventListener(final ShenyuDiscoveryConfig shenyuDiscoveryConfig,
                                                                                                   final HttpClientRegisterRepository httpClientRegisterRepository,
                                                                                                   final ClientRegisterConfig clientRegisterConfig) {
        return new ClientDiscoveryConfigRefreshedEventListener(shenyuDiscoveryConfig, httpClientRegisterRepository, clientRegisterConfig, PluginEnum.WEB_SOCKET);
    }

    /**
     * InstanceRegisterListener.
     *
     * @param eventListener         eventListener
     * @param shenyuDiscoveryConfig discoveryConfig
     * @return InstanceRegisterListener
     */
    @Bean("websocketInstanceRegisterListener")
    @ConditionalOnBean(ShenyuDiscoveryConfig.class)
    public InstanceRegisterListener instanceRegisterListener(final SpringWebSocketClientEventListener eventListener, final ShenyuDiscoveryConfig shenyuDiscoveryConfig) {
        DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
        discoveryUpstreamData.setProtocol(ShenyuClientConstants.WS);
        discoveryUpstreamData.setStatus(0);
        String weight = shenyuDiscoveryConfig.getProps().getOrDefault("discoveryUpstream.weight", "10").toString();
        discoveryUpstreamData.setWeight(Integer.parseInt(weight));
        discoveryUpstreamData.setUrl(eventListener.getHost() + ":" + eventListener.getPort());
        return new InstanceRegisterListener(discoveryUpstreamData, shenyuDiscoveryConfig);
    }

}
