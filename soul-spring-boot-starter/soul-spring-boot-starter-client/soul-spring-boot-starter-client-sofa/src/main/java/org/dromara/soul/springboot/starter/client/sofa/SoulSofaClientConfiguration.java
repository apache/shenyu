package org.dromara.soul.springboot.starter.client.sofa;

import org.dromara.soul.client.sofa.SofaServiceBeanPostProcessor;
import org.dromara.soul.client.sofa.common.config.SofaConfig;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Sofa type client bean postprocessor.
 *
 * @author tydhot
 */
@Configuration
public class SoulSofaClientConfiguration {
    /**
     * Sofa service bean post processor sofa service bean post processor.
     *
     * @param sofaConfig the sofa config
     * @return the sofa service bean post processor
     */
    @Bean
    public SofaServiceBeanPostProcessor sofaServiceBeanPostProcessor(final SofaConfig sofaConfig) {
        return new SofaServiceBeanPostProcessor(sofaConfig);
    }

    /**
     * Sofa config sofa config.
     *
     * @return the sofa config
     */
    @Bean
    @ConfigurationProperties(prefix = "soul.sofa")
    public SofaConfig sofaConfig() {
        return new SofaConfig();
    }
}
