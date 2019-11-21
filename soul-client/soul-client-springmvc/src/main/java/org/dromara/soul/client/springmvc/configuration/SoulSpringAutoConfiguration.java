package org.dromara.soul.client.springmvc.configuration;

import org.dromara.soul.client.springmvc.config.SoulHttpConfig;
import org.dromara.soul.client.springmvc.init.ApplicationStartListener;
import org.dromara.soul.client.springmvc.init.SoulClientBeanPostProcessor;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.env.Environment;

/**
 * @author xiaoyu
 */
@Configuration
@EnableConfigurationProperties({SoulHttpConfig.class})
@Import(HttpServerConfig.class)
public class SoulSpringAutoConfiguration {

    private final Environment env;

    private final SoulHttpConfig soulHttpConfig;

    public SoulSpringAutoConfiguration(Environment env, SoulHttpConfig soulHttpConfig) {
        this.env = env;
        this.soulHttpConfig = soulHttpConfig;
    }

    @Bean
    public SoulClientBeanPostProcessor soulClientBeanPostProcessor() {
        return new SoulClientBeanPostProcessor(env, soulHttpConfig);
    }

    @Bean
    public ApplicationStartListener applicationStartListener() {
        return new ApplicationStartListener(soulHttpConfig);
    }
}
