package org.apache.shenyu.springboot.starter.plugin.basic.auth;

/**
 * @author romic
 * @date 2022/7/19
 */

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.basic.auth.BasicAuthPlugin;
import org.apache.shenyu.plugin.basic.auth.handle.BasicAuthPluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Jwt plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.basic-auth.enabled"}, havingValue = "true", matchIfMissing = true)
public class BasicAuthPluginConfiguration {
    /**
     * GeneralContext plugin.
     *
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin basicAuthPlugin() {
        return new BasicAuthPlugin();
    }

    /**
     * GeneralContext plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler basicAuthPluginDataHandler() {
        return new BasicAuthPluginDataHandler();
    }
}
