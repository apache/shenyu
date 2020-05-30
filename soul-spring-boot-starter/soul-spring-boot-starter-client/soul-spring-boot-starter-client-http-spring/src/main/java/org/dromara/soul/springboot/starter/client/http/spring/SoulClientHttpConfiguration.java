package org.dromara.soul.springboot.starter.client.http.spring;

import org.dromara.soul.client.http.spring.SpringHttpClientBeanPostProcessor;
import org.dromara.soul.client.http.spring.config.SoulHttpConfig;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Soul client http configuration.
 *
 * @author xiaoyu
 */
@Configuration
public class SoulClientHttpConfiguration {
    
    /**
     * Spring http client bean post processor spring http client bean post processor.
     *
     * @param soulHttpConfig the soul http config
     * @return the spring http client bean post processor
     */
    @Bean
    public SpringHttpClientBeanPostProcessor springHttpClientBeanPostProcessor(final SoulHttpConfig soulHttpConfig) {
        return new SpringHttpClientBeanPostProcessor(soulHttpConfig);
    }
    
    /**
     * Soul http config soul http config.
     *
     * @return the soul http config
     */
    @Bean
    @ConfigurationProperties(prefix = "soul.http")
    public SoulHttpConfig soulHttpConfig() {
        return new SoulHttpConfig();
    }
}
