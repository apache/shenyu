package org.dromara.soul.client.dubbo.configuration;

import org.dromara.soul.client.dubbo.config.DubboConfig;
import org.dromara.soul.client.dubbo.spring.DubboListener;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Dubbo client auto configuration.
 * 使用的时候，请先指定:
 * soul.dubbo.url = localhost:8080/soul/register
 * soul.dubbo.contextPath = /test
 * @author xiaoyu
 */
@Configuration
@EnableConfigurationProperties({DubboConfig.class})
public class DubboClientAutoConfiguration {

    private final DubboConfig dubboConfig;

    public DubboClientAutoConfiguration(final DubboConfig dubboConfig) {
        this.dubboConfig = dubboConfig;
    }

    /**
     * Dubbo listener dubbo listener.
     *
     * @return the dubbo listener
     */
    @Bean
    public DubboListener dubboListener() {
        return new DubboListener(dubboConfig);
    }
}
