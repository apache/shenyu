package org.apache.shenyu.springboot.starter.plugin.ai.transformer.request;

import org.apache.shenyu.plugin.ai.transformer.request.AiRequestTransformerPlugin;
import org.apache.shenyu.plugin.ai.transformer.request.handler.AiRequestTransformerPluginHandler;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.ServerCodecConfigurer;

/**
 * The type ai Request Transformer plugin configuration.
 */

@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.ai.transformer.request.enabled"}, havingValue = "true", matchIfMissing = true)
public class AiRequestTransformerPluginConfiguration {

    /**
     * Ai Request Transformer plugin.
     *
     * @param configurer the configurer
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin aiProxyPlugin(final ServerCodecConfigurer configurer) {
        return new AiRequestTransformerPlugin(configurer.getReaders());
    }

    /**
     * Ai Request Transformer plugin handler.
     *
     * @return the shenyu plugin handler
     */
    @Bean
    public AiRequestTransformerPluginHandler aiProxyPluginHandler() {
        return new AiRequestTransformerPluginHandler();
    }

}
