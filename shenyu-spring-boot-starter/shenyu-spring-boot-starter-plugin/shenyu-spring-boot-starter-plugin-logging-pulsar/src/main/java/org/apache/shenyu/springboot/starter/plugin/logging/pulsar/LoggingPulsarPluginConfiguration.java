package org.apache.shenyu.springboot.starter.plugin.logging.pulsar;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.logging.pulsar.LoggingPulsarPlugin;
import org.apache.shenyu.plugin.logging.pulsar.handler.LoggingPulsarPluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * config logging Pulsar plugin.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.logging-pulsar.enabled"}, havingValue = "true", matchIfMissing = true)
public class LoggingPulsarPluginConfiguration {

    /**
     * logging pulsar plugin data handler.
     *
     * @return logging pulsar PluginDataHandler
     */
    @Bean
    public PluginDataHandler loggingPulsarPluginDataHandler() {
        return new LoggingPulsarPluginDataHandler();
    }

    /**
     * Logging Pulsar plugin.
     *
     * @return LoggingPulsarPlugin
     */
    @Bean
    public ShenyuPlugin loggingPulsarPlugin() {
        return new LoggingPulsarPlugin();
    }
}
