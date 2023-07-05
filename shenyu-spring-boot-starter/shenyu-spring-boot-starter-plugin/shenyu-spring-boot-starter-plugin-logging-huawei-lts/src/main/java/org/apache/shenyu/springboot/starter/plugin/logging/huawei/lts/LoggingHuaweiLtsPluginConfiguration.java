package org.apache.shenyu.springboot.starter.plugin.logging.huawei.lts;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.huawei.lts.LoggingHuaweiLtsPlugin;
import org.apache.shenyu.plugin.huawei.lts.handler.LoggingHuaweiLtsPluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Logging Huawei lts plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.logging-huawei-lts.enabled"}, havingValue = "true", matchIfMissing = true)
public class LoggingHuaweiLtsPluginConfiguration {
    /**
     * logging Huawei lts plugin data handler.
     *
     * @return logging Huawei lts PluginDataHandler
     */
    @Bean
    public PluginDataHandler loggingHuaweiLtsPluginDataHandler() {
        return new LoggingHuaweiLtsPluginDataHandler();
    }

    /**
     * Logging Huawei lts plugin.
     *
     * @return LoggingHuaweiLtsPlugin
     */
    @Bean
    public ShenyuPlugin loggingHuaweiLtsPlugin() {
        return new LoggingHuaweiLtsPlugin();
    }

}
