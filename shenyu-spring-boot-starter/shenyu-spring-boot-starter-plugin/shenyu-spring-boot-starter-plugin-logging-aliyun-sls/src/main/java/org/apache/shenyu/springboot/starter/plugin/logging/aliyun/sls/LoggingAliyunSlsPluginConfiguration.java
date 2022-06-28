package org.apache.shenyu.springboot.starter.plugin.logging.aliyun.sls;


import org.apache.shenyu.plugin.aliyun.sls.LoggingAliYunSlsPlugin;
import org.apache.shenyu.plugin.aliyun.sls.handler.LoggingAliYunSlsPluginDataHandler;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Logging Aliyun sls plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.logging-aliyun-sls.enabled"}, havingValue = "true", matchIfMissing = true)
public class LoggingAliyunSlsPluginConfiguration {

    /**
     * logging aliyun sls plugin data handler.
     * @return logging aliyun sls PluginDataHandler
     */
    @Bean
    public PluginDataHandler loggingAliyunSlsPluginDataHandler() {
        return new LoggingAliYunSlsPluginDataHandler();
    }

    /**
     * Logging Aliyun sls plugin.
     * @return LoggingElasticSearch
     */
    @Bean
    public ShenyuPlugin loggingAliyunSlsPlugin() {
        return new LoggingAliYunSlsPlugin();
    }
}
