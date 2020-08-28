package org.dromara.soul.springboot.starter.plugin.oauth;

import org.dromara.soul.plugin.oauth.OAuthPlugin;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class OAuthPluginConfiguration {

    @Bean
    public OAuthPlugin oauthPlugin() { return new OAuthPlugin(); }
}
