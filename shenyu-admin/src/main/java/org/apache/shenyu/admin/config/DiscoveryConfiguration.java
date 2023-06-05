package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.discovery.DefaultDiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class DiscoveryConfiguration {

    /**
     * discoveryProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     * @param proxySelectorMapper     proxySelectorMapper
     * @return DiscoveryProcessor
     */
    @Bean
    public DiscoveryProcessor discoveryProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper, final ProxySelectorMapper proxySelectorMapper) {
        return new DefaultDiscoveryProcessor(discoveryUpstreamMapper, proxySelectorMapper);
    }
}
