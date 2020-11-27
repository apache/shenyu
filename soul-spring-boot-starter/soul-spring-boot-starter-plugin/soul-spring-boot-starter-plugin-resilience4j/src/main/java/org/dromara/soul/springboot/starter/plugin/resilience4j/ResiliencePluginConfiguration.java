package org.dromara.soul.springboot.starter.plugin.resilience4j;

import org.dromara.soul.plugin.resilience4j.ResilencePlugin;
import org.dromara.soul.plugin.resilience4j.executor.CombinedExecutor;
import org.dromara.soul.plugin.resilience4j.executor.RatelimiterExecutor;
import org.dromara.soul.plugin.resilience4j.handler.ResilienceHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Resilience4j plugin configuration.
 *
 * @author zhanglei
 */
@Configuration
public class ResiliencePluginConfiguration {


    /**
     * Resilience4j plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public ResilencePlugin resilencePlugin() {
        return new ResilencePlugin(new CombinedExecutor(), new RatelimiterExecutor());
    }


    /**
     * Resilience4j handler.
     *
     * @return ResilienceHandler
     */
    @Bean
    public ResilienceHandler resilienceHandler() {
        return new ResilienceHandler();
    }


}
