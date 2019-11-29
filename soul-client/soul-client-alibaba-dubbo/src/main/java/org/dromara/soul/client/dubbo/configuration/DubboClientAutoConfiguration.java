package org.dromara.soul.client.dubbo.configuration;

import org.dromara.soul.client.dubbo.config.DubboConfig;
import org.dromara.soul.client.dubbo.spring.DubboServiceBeanPostProcessor;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Dubbo client auto configuration.
 * 使用的时候，请先指定:
 * soul.dubbo.adminUrl = http://localhost:8080/soul/register
 * soul.dubbo.contextPath = /test
 *
 * @author xiaoyu
 */
@Configuration
@EnableConfigurationProperties({DubboConfig.class})
public class DubboClientAutoConfiguration {

    private final DubboConfig dubboConfig;

    /**
     * Instantiates a new Dubbo client auto configuration.
     *
     * @param dubboConfig the dubbo config
     */
    public DubboClientAutoConfiguration(final DubboConfig dubboConfig) {
        this.dubboConfig = dubboConfig;
    }


    /**
     * Dubbo service bean post processor dubbo service bean post processor.
     *
     * @return the dubbo service bean post processor
     */
    @Bean
    public DubboServiceBeanPostProcessor dubboServiceBeanPostProcessor() {
        return new DubboServiceBeanPostProcessor(dubboConfig);
    }
}
