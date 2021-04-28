package org.dromara.soul.springboot.starter.plugin.debug;

import org.dromara.soul.plugin.api.SoulPlugin;
import org.dromara.soul.plugin.debug.DebugPlugin;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Debug plugin configuration.
 *
 * @author xuxd
 **/
@Configuration
public class DebugPluginConfiguration {

    /**
     * Debug plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin debugPlugin() {
        return new DebugPlugin();
    }

}
