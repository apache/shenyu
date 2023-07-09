package org.apache.shenyu.bootstrap;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.huawei.lts.LoggingHuaweiLtsPlugin;
import org.apache.shenyu.plugin.huawei.lts.handler.LoggingHuaweiLtsPluginDataHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

@Component
public class Test {
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
