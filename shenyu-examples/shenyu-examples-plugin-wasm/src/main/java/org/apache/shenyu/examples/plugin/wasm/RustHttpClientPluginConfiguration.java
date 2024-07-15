package org.apache.shenyu.examples.plugin.wasm;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * In fact, this type should be separated into a separate module.
 * <p>
 * Here just for example.
 */
@Configuration
public class RustHttpClientPluginConfiguration {

    /**
     * ShenYu rust http client.
     *
     * @return the http client
     */
    @Bean
    public RustHttpClientPlugin rustHttpClientPlugin() {
        return new RustHttpClientPlugin();
    }
}
